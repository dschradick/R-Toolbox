########## LOGISTISCHE REGRESSION - THRESHOLD DURCH MIT PAYOFF-MATRIX
library(tidyverse)
library(corrplot)
library(rms)
library(MASS) # für stepAIC

###### Beispiel: Churn Modeling
# Wahrscheinlich des Churns = P(Y=1)
# log odds                  = log( P(Y=1)/P(Y=0) ) = \beta_0 + \sigma_{p=1}^P \beta_p*x_p
# odds                      = P(Y=1)/P(Y=0)   = e^Z   ,wobei z = \beta_0 + \sigma_{p=1}^P \beta_p*x_p
# => Wahrscheinlich des Churns = P(Y=1) = e^Z / 1+ e^Z  

defaultData = read.csv2("~/Documents/Data/defaultData.csv")
str(defaultData)

## Target-Variable
ggplot(defaultData,aes(x = PaymentDefault)) +
  geom_histogram(stat = "count")

formula <- PaymentDefault ~ limitBal + sex + education + marriage +
                            age + pay1 + pay2 + pay3 + pay4 + pay5 + pay6 + billAmt1 + 
                            billAmt2 + billAmt3 + billAmt4 + billAmt5 + billAmt6 + payAmt1 + 
                            payAmt2 + payAmt3 + payAmt4 + payAmt5 + payAmt6
logitModelFull <- glm(formula = formula, family = binomial, data = defaultData)
summary(logitModelFull)

## Reduziertes Modell
logitModelNew <- stepAIC(logitModelFull, trace = 0) 
summary(logitModelNew) 

### Odds betrachten
# ohne exp():
#     => nur Richtung des effekts direkt folgerbar durch vorzeichen
# mit exp(): 
#    => effekt auf die odds
coefsexp <- coef(logitModelFull) %>% exp() %>% round(2)
coefsexp


### Evaluation
# Pseudo R^2: McFadden, Cox&Snell, Nagelkerke
# Interpretation: 
# Moderat    wenn > 0.2
# Gut        wenn > 0.4
# Sehr gut   wenn > 0.5
library(descr)
LogRegR2(logitModelNew)

### Warscheinlichkeit vorhersagen
library(SDMTools)   # für confusionmatrix

defaultData$predFull <- predict(logitModelFull, type = "response", na.action = na.exclude)
confMatrixModelFull <- confusion.matrix(defaultData$PaymentDefault, 
                                        defaultData$predFull, 
                                        threshold = 0.5) # => hier möglich!!
## Konfusionsmatrix anzeigen
confMatrixModelFull
## Accuracy 
accuracyFull <- sum(diag(confMatrixModelFull)) / sum(confMatrixModelFull)
accuracyFull

## Für das zweite Modell
defaultData$predNew <- predict(logitModelNew, type = "response", na.action = na.exclude)
(confMatrixModelNew <- confusion.matrix(defaultData$PaymentDefault, defaultData$predNew, threshold = 0.5))
(accuracyNew <- sum(diag(confMatrixModelNew)) / sum(confMatrixModelNew))


#### Threshold bestimmen mit Potentiellem Payoff
# Matrix für potentielle Payoffs
# Bsp. Coupon als Maßnahme, dass er zurückkommt
# Prediction / Truth      returnCustomer = 0   returnCustomer = 1
# returnCustomer = 0              5                  -15
# ReturnCustomer = 1              0                    0
#
# 1. Wenn customer zurückkommt, weil er auf basis der vorhersage einen Coupon bekommt, 
#    dann Payoff durchschnittlich $5
# 2. Fälschlicherweise customer als churner vorhersagen, 
#    welcher sowieso wiederkommt führt zu einem Verlust von durchschnittlich $15
# 3. Vorhersage, dass Customer zurückkommmt erzeugt keine direkten Kosten
#
# => PAYOFF:  payoff = 5 * true negative - 15 * false negative
# => threshold ändern ändern Payoff
# Bsp:   Threshold  Accuracy     Payoff
#        0.5	       0.817      	60975
#        0.4	       0.815	      62180
#        0.3	       0 794	      65740   => bester Payoff bei 0.3
#        0.2       	 0.668	      65670
#        0.1	       0.241	      10550

## Beispielszenario:
# payoff = 250 * true negative - 1000 * false negative

## Konfusionsmatrix + Payoff erstellen
# dataframe mit threshold values und noch leerer payoff spalte
payoffMatrix <- data.frame(threshold = seq(from = 0.1, to = 0.5, by = 0.1), payoff = NA) 
payoffMatrix 

# Berechnen der Konfusionsmatrix für die verschiedenen thresholds
for(i in 1:length(payoffMatrix$threshold)) {
  confMatrix <- confusion.matrix(defaultData$PaymentDefault,
                                 defaultData$predNew, 
                                 threshold = payoffMatrix$threshold[i])
  # Payoff berechnen
  payoffMatrix$payoff[i] <- confMatrix[1,1]*250 + confMatrix[1,2]*(-1000)
}
payoffMatrix  
# => Optimal bei 0.3


### Evaluation: Train/Test-Split: Out-of-Sample Error berechnen
## Split Train/Test-set
set.seed(0) 
defaultData$isTrain <- rbinom(nrow(defaultData), 1, 0.66)
train <- subset(defaultData, defaultData$isTrain == 1)
test  <- subset(defaultData, defaultData$isTrain == 0)

## Model fitten
logitTrainNew <- glm(formula, family = binomial, data = train) # Modeling  
test$predNew <- predict(logitTrainNew, type = "response", newdata = test) # Predictions

## Konfusionsmatrix + Accuracy
confMatrixModelNew <- confusion.matrix(test$PaymentDefault, test$predNew, threshold = 0.3) 
sum(diag(confMatrixModelNew)) / sum(confMatrixModelNew) 


### Evaluation: k-fold Cross-Validation
library(boot)
# Accuracy Funktion
costAcc <- function(r, pi = 0) {
  cm <- confusion.matrix(r, pi, threshold = 0.3)
  acc <- sum(diag(cm)) / sum(cm)
  return(acc)
}

# Cross-Validated accuracy
set.seed(0)
cv.glm(defaultData, logitModelNew, cost = costAcc, K = 6)$delta[1]


