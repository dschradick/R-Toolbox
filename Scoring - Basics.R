########## SCORING
library(tidyverse)
library(gmodels)
library(broom)
library(rpart)

#### Daten laden
loan_data <- readRDS("~/Documents/Data/loan_data_ch1.rds")

# grades: A-G (gut bis schlecht)
head(loan_data)
loan_data
glimpse(loan_data)

#### EDA für Scoring

## KATEGORISCHE mit CrossTable betrachten 
# Anzahl der Fälle und Proportionen
CrossTable(loan_data$home_ownership)
# Zusammenhang mit der Response
CrossTable(loan_data$home_ownership,loan_data$loan_status,
           prop.r = T, prop.c = F, prop.t = F, prop.chisq = F)
# => default-rate für OTHER höher als für MORTGAGE

## Kontinuierliche Variablen 
hist(loan_data$int_rate, main="Histogram of interest rate", xlab="Interest rate")
hist(loan_data$annual_inc) # Nur ein Balken
hist_income <- hist(loan_data$annual_inc)
hist_income$breaks
n_breaks <- sqrt(nrow(loan_data)) # Rule of thumb 
hist_income <- hist(loan_data$annual_inc, breaks = n_breaks) # besser, aber viel leer
plot(loan_data$annual_inc) # => outlier

## Outlier entfernen
# mittels Expertenwissen
index_outlier_expert <- which(loan_data$annual_inc > 3000000)
loan_data_expert <- loan_data[-index_outlier_expert,]
hist(loan_data_expert$annual_inc,breaks=n_breaks)
# Rule of thumb mittels IQR: Q3 + 1.5 * IQR 
# hier nur positive weggeschnitten
outlier_cutoff <- quantile(loan_data$annual_inc, 0.75) + 1.5 * IQR(loan_data$annual_inc)
index_outlier_ROT <- which(loan_data$annual_inc > outlier_cutoff)
loan_data_ROT <- loan_data[-index_outlier_ROT,]
hist(loan_data_ROT$annual_inc,breaks=n_breaks)


## Missing Values
# Anzahl NAs anzeigen
summary(loan_data_re$emp_length)
# NAs läschen
index_NA <- which(is.na(loan_data$emp_length))
loan_data_no_na <- loan_data[-c(index_NA),]
# Spalte löschen
loan_data_delete_employ <- loan_data
loan_data_delete_employ$emp_length <- NULL
# Ersetzen mit Median (bei kategorisch häufigste kategorie)
loan_data_replace <- loan_data
loan_data_replace$emp_length[index_NA] = median(loan_data$emp_length, na.rm = T)
# Behalten (bei kategorisch, NA als kategorie hinzufügen)

#### Coarse classification 
loan_data$ir_cat <- rep(NA, length(loan_data$int_rate))
loan_data$ir_cat[which(loan_data$int_rate <= 8)] <- "0-8"
loan_data$ir_cat[which(loan_data$int_rate > 8 & loan_data$int_rate <= 11)] <- "8-11"
loan_data$ir_cat[which(loan_data$int_rate > 11 & loan_data$int_rate <= 13.5)] <- "11-13.5"
loan_data$ir_cat[which(loan_data$int_rate > 13.5)] <- "13.5+"
loan_data$ir_cat[which(is.na(loan_data$int_rate))] <- "Missing"
loan_data$ir_cat <- as.factor(loan_data$ir_cat)
plot(loan_data$ir_cat)

## Weitere Coarse Classification
loan_data$emp_cat <- rep(NA, length(loan_data$emp_length))
loan_data$emp_cat[which(loan_data$emp_length <= 15)] <- "0-15"
loan_data$emp_cat[which(loan_data$emp_length > 15 & loan_data$emp_length <= 30)] <- "15-30"
loan_data$emp_cat[which(loan_data$emp_length > 30 & loan_data$emp_length <= 45)] <- "30-45"
loan_data$emp_cat[which(loan_data$emp_length > 45)] <- "45+"
loan_data$emp_cat[which(is.na(loan_data$emp_length))] <- "Missing"
loan_data$emp_cat <- as.factor(loan_data$emp_cat)



#### Train / Test-Split
set.seed(0)
index_train <- sample(1:nrow(loan_data),2 / 3 * nrow(loan_data))
training_set <- loan_data[index_train, ]
test_set <- loan_data[-index_train, ]

## LOGISTISCHE REGRESSION
# Output des Modells zwischen 0 und 1 
# => zur Modellierung der Wahrscheinlich von Default
# Entspricht der Wahrscheinlichkeit, dass 
#    loan_status = 1, abhängig von den Variablen x_1,..,x_m
# wobei x_1,..,x_m = Variablen, 
#       beta_0,..., beta_m = zu schätzende Parameter 
#       beta_0 + beta_1*x_1 + ... + beta_m*x_m = Linear Predictor
# P(loan_stats = 1 | x_1,...,x_m) = 1 / (1 + e^(-(beta_0 + beta_1*x_1 + ... + beta_m*x_m)))

## Model Fitting
log_model <- glm(loan_status ~ age, family='binomial', data = training_set)
log_model
# P(loan_status = 1 | age) => 1 / (1 + e^(-(beta_0 + beta_1 * age)))


## Interpretation
# P(loan_stats = 1 | x_1,...,x_m) / P(loan_stats = 0 | x_1,...,x_m) 
#    = e^(beta_0 + beta_1*x_1 + ... + beta_m*x_m) 
#    = Odds/Chance für loan_stats = 1
# x_j um 1 erhöht => Odds werden mit e^beta_j multipliziert
# Dh. Wenn Koeffizient
# Negativ: beta_j < 0, so ist e^beta_j < 1 ===> Odds FÄLLT wenn x_j steigt
# Positiv: beta_j > 0, so ist e^beta_j < 1 ===> Odds STEIGT wenn x_j steigt
#
# Bsp: Wenn age um 1 ansteigt => Odds werden mit e^(-0.008824) = 0.9 multipliziert
# Ein extra Jahr Alter verringert die Wahrscheinnlichkeit von Default um ca. 1%

## PREDICTION
log_model <- glm(loan_status ~ ., family='binomial', data = training_set)
log_model <- glm(loan_status ~ age + home_ownership, family='binomial', data = training_set)
log_model
summary(log_model) # => significance levels und confidence intervals
exp(log_model$coefficients) # odds in favor of default change by a multiple of ...

# P(loan_status = 1 | age, home_ownership) 
# Alle ausser Referenz-Kategorie
# => 1 / (1 + e^(-(beta_0 + beta_1 * age + beta_2 * OTHER + beta_3 * OWN + beta_4 * RENT)))
#
# Bsp: P(loan_status = 1 | age = 33, home_ownership = RENT) 
# = Wahrscheinlich von Default, wenn 33 Jahre und Mieter ist ====> Einsetzen 
# = 0,1159 = 11,5%
test_case <- as.data.frame(test_set[1,])
predict(log_model, newdata=test_case) # Ausgabe: Linear predictor
predict(log_model, newdata=test_case, type='response') # Ausgabe: Linear predictor
predictions <- predict(log_model, newdata=test_set, type='response')
range(predictions)

## Evaluation
# Cutoff / Threshold-Value
# Problem: Output ist Wahrscheinlichkeit 
# => Cutoff zwischen 0 und 1 wählen
#
# Bsp: binärer predictions-vector mit cut-off of 15%
pred_cutoff_15 <- ifelse(predictions > 0.15, 1, 0)
# Confusion-Matrix
(conf_matrix <- table(test_set$loan_status,pred_cutoff_15))
# Accuracy
sum(diag(conf_matrix)) / nrow(test_set)
# Sensitivity & Specificty 
library(caret)
sensitivity(conf_matrix)
specificity(conf_matrix)

#### Decision Trees
# Vorteil: Interpretierbarkeit

## Splitting Entscheidung
# Measure für Impurity => Ziel ist es die Impurity in jedem Knoten zu minimieren
# GINI: 2 * proportion(default) * proportion(non-default)
# Bsp: 
# Root: 250/250 => 2 * 250/500 * 250/500 = 0.5  # dann split age > 40
# 1 = Default     = 170/100 => 2 * 170/270 * 100/500 = 0.4664
# 0 = Non-Default =  80/150 => 2 *  80/230 * 150/230 = 0.4536
# PURITY-GAIN: wenn man von root node zu den childs N1 und N2 geht
# Berechnet indem man die gewichteten Gini-Measures von N1 und N2 von Root Node abzieht
# Gain = Gini_R - prop(cases in N1) * Gini_N1 - prop(cases in N2) * Gini_N1
#        0.5 - 270/500 * 0.4664 - 230/500 * 0.4536 = 0.039488
# => Algorithmus verwendet den Split mit höchstem PURITY gain


## rpart
# Problem: schwer gute credit risk modelle mit decsion trees zu bauen
# Grund: Unbalanced classes - nur sehr wenige defaults

fit <- rpart(loan_status ~ ., method = "class", data = training_set)
plot(fit) # => just root, weil - analog zu cut-off bei LR - höchste Accurary erzieht, wenn alle als non-default predicted werden

#### Abhilfe bei Unbalanced Classes
# 1a. Undersampling (der überrepräsentierten Gruppe - non-default) 
# 1b. Oversampling (der unterrepräsentierten Grupppe - defaults)
#  => löst Problem der Accurary
#  => darf nur auf das Training-Set angewandt werden
# 2. Prior Wahrscheinlichkeit in rpart Funktion verändern
#  Standard: Proportion der Klassen im Trainingset
#  => Wahrscheinlichkeit für Default erhöhen - so legt rpart höhere Priorität auf Default
# 3. Loss Matrix
#  Erlaubt verschiedene Kosten für die Missklassifikation anzugeben
#  z.B. Kosten Missklassifikation von Default als Non-Default (vs andersrum)
#   => höhere Kosten für Missklassifikation von Default 
#      = höhere Priorität von korrekter Klassifikation von Defaults
## ===> Ausprobieren und Model-Validation um zu prüfen, welches im Fall am besten funktioniert

## Undersampling 
# Hier größe der major class auf größe der minor reduzieren
set.seed(0)
defaults <- subset(training_set, loan_status == 1)
non_defaults_undersampled <- sample_n(subset(training_set, loan_status == 0), nrow(defaults))
undersampled_training_set <- rbind(defaults,non_defaults_undersampled)
nrow(undersampled_training_set)

tree_undersample <- rpart(loan_status ~ ., method = "class",
                          data =  undersampled_training_set, 
                          control = rpart.control(cp = 0.001))
plot(tree_undersample,uniform=TRUE)
text(tree_undersample)

## Prior
# Indirekte Weise die Wichtigkeit der Fehlklassifkation von jeder Klasse zu setzen
# parms = list(prior=c(non_default_proportion, default_proportion))
tree_prior <- rpart(loan_status ~ ., method = "class",
                    parms = list(prior=c(0.7, 0.3)),
                    control = rpart.control(cp = 0.001),
                    data = training_set)
plot(tree_prior,uniform = T)
text(tree_prior)
rsq.rpart(tree_prior)
summary(tree_prior)

## Loss-Matrix
# loss = matrix(c(0, cost_def_as_nondef, cost_nondef_as_def, 0), ncol=2))
# Bsp: Penalization 10 größer wenn Fehlklassifikation von tatsächlichem default als non-defaults
#      vs non-default als default
tree_loss_matrix <- rpart(loan_status ~ ., method = "class",
                          parms = list(loss = matrix(c(0, 10, 1, 0), ncol=2)),
                          control = rpart.control(cp = 0.001),
                          data =  training_set)
plot(tree_prior,uniform = T)
text(tree_prior,use.n = T)

## Pruning
# Resultierende Bäume sind sehr gross
# Problem: Overfitting & schwer zu interpretieren 
# Lösung: printcp() und plotcp() zum pruning

# cp, which is the complexity parameter, is the threshold value 
# for a decrease in overall lack of fit for any split. If cp is not met, 
# further splits will no longer be pursued. cp's default value is 0.01, 
# but for complex problems, it is advised to relax cp

# printcp gibt Auskunft wie der Baum bei mehreren Splits(nsplits) wächst 
# => Ziel complexity bestimmen, welche Cross-Validation error minimiert (xerror)


plotcp(tree_prior) # cv-error-rate als Funktion des complexity parameter
printcp(tree_prior) # Identifikation für welchen complexity parameter cv-error-rate minimal
index <- which.min(tree_prior$cptable[ , "xerror"]) 
tree_min <- tree_prior$cptable[index, "CP"] #  complexity parameter vom baum mit minimaler cv-error-rate
ptree_prior <- prune(tree_prior, cp = tree_min)
library(rpart.plot)
plot(ptree_prior)
text(ptree_prior)
prp(ptree_prior) # plot des puned trees 

# von tree_loss_matrix
plotcp(tree_loss_matrix) 
ptree_loss_matrix <- prune(tree_loss_matrix,cp=0.0012788)
prp(ptree_loss_matrix, extra = 1)


### Weitere sehr nützliche Argumente: case-weights, minsplit, minbucket
## Case-Weights 
# (also für jede) Beobachtung
# weights: erlaubt caseweights für jedes der weights im trainingset (z.B um 
#  => Gewichtung von Default erhöhen, um Problem der Imbalance abzuschwächen

case_weights <- training_set %>% 
  mutate(weight = ifelse(loan_status == 1, 4, 1)) %>%
  select(weight)
case_weights <- as.vector(case_weights$weight)


# rpart.control
# minsplit: minimale Anzahl von Observations in einem Konten, damit ein split des Knotens versucht wird
#   Standard: 20 => für unbalanced data sinnvoll zu verringern
# minbucket: minimale Anzahl von Obervations in Leaf-Node
#   Standard: 1/3 * minsplit => möglicherweise verringen
# ===> verringern dieser Werte kann zu Overfitting führen

set.seed(345)
tree_weights <- rpart(loan_status ~ ., method = "class",
                      data = training_set,
                      weights = case_weights,
                      control = rpart.control(minsplit = 5, minbucket = 2, cp = 0.001))

prp(tree_weights,extra=1)
plotcp(tree_weights)
printcp(tree_weights)
index <- which.min(tree_weights$cptable[ , "xerror"])
tree_min <- tree_weights$cp[index, "CP"]
ptree_weights <- prune(tree_weights, cp = tree_min)
prp(ptree_weights,extra=1)

#### Confusion Matrix & Accuracy der Bäume vergleichen
# Predictions
pred_prior <- predict(ptree_prior, newdata = test_set, type = "class")
pred_loss_matrix <- predict(ptree_loss_matrix, newdata = test_set, type = "class")
pred_weights <- predict(ptree_weights, newdata = test_set, type = "class")

# confusion matrices 
confmat_prior <- table(test_set$loan_status, pred_prior)
confmat_loss_matrix <- table(test_set$loan_status, pred_loss_matrix)
confmat_weights <- table(test_set$loan_status, pred_weights)

# Accuracy
acc_prior <- sum(diag(confmat_prior)) / nrow(test_set)
acc_loss_matrix <- sum(diag(confmat_loss_matrix)) / nrow(test_set)
acc_weights <- sum(diag(confmat_weights)) / nrow(test_set)

acc_prior ; acc_loss_matrix ; acc_weights



#### CUT-OFF BESTIMMEN

####  Bad-Rate zur festen Akzeptanz-Rate
# z.B 80% der Kunden sollen akzeptiert, basierend auf der fitted probabilty
# => die 20% mit der höchsten Wahrscheinlichkeit werden rejected
#    Um cut-off zu bestimmen 80% quantil des predictions-vektor des test-sets
# Bad-Rate: Wieviele würden bei diesem gewählten cut-off defaulten 

# Strategy Table:
# ist Tabelle für verschiedene acceptance-rates mit dazugehörigem cut-off & bad-rate
# accept_rate, cutoff, bad_rate
# => erlaubt es acceptance-rate anzupassen basierend darauf 
#    welchen Prozentsatz an bad-loans sie sich erlauben können - z.B. 8% Badrate

# predictions für default wahrscheinlichkeit
prob_default_prior <- predict(ptree_prior, newdata = test_set)[ ,2]

# cutoff für acceptance rate 80%
cutoff_prior <- quantile(prob_default_prior, 0.8)
bin_pred_prior_80 <- ifelse(prob_default_prior > cutoff_prior, 1, 0)
# tatsächlicher default status für akzeptierte loans
accepted_status_prior_80 <- test_set$loan_status[bin_pred_prior_80 == 0]
bad_rate <- sum(accepted_status_prior_80) / length(accepted_status_prior_80)
bad_rate

## Strategy-Curve: 
# Bad-Rate als Funktion der Acceptance-Rate
strategy_bank <- function(prob_of_def){
  cutoff = rep(NA, 21)
  bad_rate = rep(NA, 21)
  accept_rate = seq(1,0,by=-0.05)
  for (i in 1:21){
    cutoff[i] = quantile(prob_of_def,accept_rate[i])
    pred_i = ifelse(prob_of_def> cutoff[i], 1, 0)
    pred_as_good = test_set$loan_status[pred_i==0]
    bad_rate[i] = sum(pred_as_good) / length(pred_as_good)
  }
  table = cbind(accept_rate, 
                cutoff = round(cutoff,4), 
                bad_rate = round(bad_rate,4))
  return(list(table = table,
              bad_rate = bad_rate, 
              accept_rate = accept_rate, 
              cutoff = cutoff))
}

predictions_log <- predict(log_model, newdata = test_set, type='response')
strategy_log <- strategy_bank(predictions_log)
predictions_loss_matrix <- predict(ptree_loss_matrix, newdata = test_set)[,2]
strategy_loss_matrix <- strategy_bank(predictions_loss_matrix)

# Strategie Tabelle für beide Prediction-Vektoren 
strategy_log$table
strategy_loss_matrix$table

# Draw the strategy functions
par(mfrow = c(1,2))
plot(strategy_log$accept_rate, strategy_log$bad_rate, 
     type = "l", xlab = "Acceptance rate", ylab = "Bad rate", 
     lwd = 2, main = "logistic regression")

plot(strategy_loss_matrix$accept_rate, strategy_loss_matrix$bad_rate, 
     type = "l", xlab = "Acceptance rate", 
     ylab = "Bad rate", lwd = 2, main = "tree")


#### ROC-Curve
## Zu bestimmen: Was ist das beste Modell allgemein (ohne Annahme von Akzeptanz-rate)
# Problem: Accurary geht allgemein hoch wenn cut-off höher
# => für credit-scoring: sensitivity und specificity
# ROC: x: 1-specificity , y = sensitivity
# Links unten cut-off=1, rechts oben cut-off=0

library(pROC)
ROC_logit <- roc(test_set$loan_status, predictions_log)
ROC_loss_matrix <- roc(test_set$loan_status, predictions_loss_matrix)

plot(ROC_logit)
lines(ROC_loss_matrix, col = "blue")

auc(ROC_logit)
auc(ROC_loss_matrix)

## AUC-based pruning: Variable-Selection 
# bzw. welche Variablen sind wichtig um default vorherzusagen
# anstatt p-value, wenn es um die Klassifaktions-performance geht
#
# Für: Logistische Regression (trees eher nicht, weil selbst zuviele Parameter)
#
# Beginnen: Modell mit allen Variablen und dann jeweils eine entfernen
# => Auswahl des Modells mit höchstem AUC

log_4_remove_amnt <- glm(loan_status ~ grade + annual_inc + emp_cat, family = binomial, data = training_set) 
log_4_remove_grade <- glm(loan_status ~ loan_amnt  + annual_inc + emp_cat, family = binomial, data = training_set)
log_4_remove_inc <- glm(loan_status ~ loan_amnt + grade  + emp_cat , family = binomial, data = training_set)
log_4_remove_emp <- glm(loan_status ~ loan_amnt + grade + annual_inc, family = binomial, data = training_set)

pred_4_remove_amnt <- predict(log_4_remove_amnt, newdata = test_set, type = "response")
pred_4_remove_grade <- predict(log_4_remove_grade, newdata = test_set, type = "response")
pred_4_remove_inc <- predict(log_4_remove_inc, newdata = test_set, type = "response")
pred_4_remove_emp <- predict(log_4_remove_emp, newdata = test_set, type = "response")

auc(test_set$loan_status, pred_4_remove_amnt)
auc(test_set$loan_status, pred_4_remove_grade)
auc(test_set$loan_status, pred_4_remove_inc)
auc(test_set$loan_status, pred_4_remove_emp)


#### Weitere Methoden
# Discriminant Analysis, RF, SVM, NN
# Survial Analysis: Betrachtet Wahrscheinlichkeiten, welche sich über die zeit verändern
#                   Time-varying covariates können miteinbezogen werden