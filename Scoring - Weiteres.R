########## SCORING - WEITERES
library(ROCR)
library(rpart)
library(rpart.utils)
library(ggplot2)
library(gmodels)
library(deal)
library(party)
library(DAAG)
library(randomForest)
library(tidyverse)
#install.packages('DAAG','party')
setwd("~/Documents/Data/")
data <- read.csv("~/Documents/Data/german_credit.csv")

### Qualitätsbestimmung von Scoring-Modellen
# Zwei grundsätzliche Ansätze, basierend auf
# 1. CDF: Kolmogorov-Smirnov, Gini index und Lift
# 2. Likelihood Dichte: Funktionen mean difference (Mahalanobis Distanz) 
#    und informationelle Statistiken

### Vorbereitung Daten
## Einfacher Train/Test Split
# hier nur simpler Split
data$Creditability <-as.factor(data$Creditability)
d = sort(sample(nrow(data), nrow(data)*.7))
train<-data[d,]
test<-data[-d,]
#train<-subset(train,select=-Creditability)

## Coarse Classfication / Binning
# Erlaubt Erzeugung von einfachen Scorecards z.B. age: 40-60 => 100 Punkte
# Sollte nicht kontinuierliche und gebinnte Version in Datensatz haben 
# => schlechtere Performance
data$amount<-as.factor(ifelse(data$Credit.Amount<=2500,'0-2500',
                       ifelse(data$Credit.Amount<=5000,'2600-5000','5000+')))

# Daten betrachten
CrossTable(train$Creditability)
CrossTable(x=data$amount, y=data$Creditability,
           prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE)

glimpse(data)

data$amount <- NULL


### Standard Modell: Logistische Regression
# Einfaches Modell
model <- glm(Creditability ~ ., data=data, family=binomial())
summary(model)
# model <- step(m)

## ROC Kurve
# Um cut-off basierend aufgrund geschäftlichen abwägungen 
# bzgl True und False Posivitve zu wählen 
test$score <- predict(model,type='response',test)
pred <- prediction(test$score,test$Creditability)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

# AUC
auc <- performance(pred,"auc")
auc@y.values[[1]]

## CV ohne caret
result <- CVbinary(obj=model, rand=NULL, nfolds=100, print.details=TRUE)
result

## KS-Test
# Kolmogorov-Smirnov test 
# = nichtparametrischer Test um CDFs von zwei Datensätzen zu vergleichen
# KS ist max. Differenz zwischen der CDF der TP-Rate und CDF der FP-Rate
# Berechung mittels der ROC-Kurve
tpr <- attr(perf,'y.values')[[1]]
fpr <- attr(perf,'x.values')[[1]]
ks.value = max(tpr - fpr)
ks.value
# => kann als Vorschlag für Cut-Off

# Visualisieren
rate <- c(rep("True Positive Rate", length(tpr)), 
           rep("False Positive Rate", length(fpr)))

ks <- data.frame(
  data = append(tpr,fpr),
  rate = rate)

ggplot(data=ks,aes(x=data,color = rate)) +
  stat_ecdf() +
  geom_vline(xintercept = ks.value, colour = "red", lty=2) +
  xlab("x") + 
  ylab("f(x)")



## Wichtigste Terms
# Bestimmen der wichtigsten Gründe für Entscheidung
# terms => rückgabe ist matrix mit fitted values für jeden Term
f.top.k<- function(x,top=3){
  res=names(x)[order(x, decreasing = TRUE)][1:top]
  paste(res,collapse=";",sep="")
}
g <- predict(model, type='terms', test)
top.k <- apply(g,1,f.top.k,top=3)
test  <- cbind(test, top.k)
head(test$top.k)


## rpart: Entscheidungsbäume 
# Rekursive Partitionierungsmethode 
# Voteil beim Scoring: 
# - kann direkt in Enscheidungsregeln konvertiert werde
# - hilft Splits / cut-offs finden, Daten zu segmentieren,...

# Normales Tree Modell:
model.tree <- rpart(Creditability ~ . ,data=train)
plot(model.tree);text(model.tree);
test$t<-predict(model.tree,type='class',test)

test$tscore1 <- predict(model.tree,type='prob',test)
pred1 <- prediction(test$tscore1[,2],test$Creditability)
perf1 <- performance(pred,"tpr","fpr")

# Prior Probabilities:
# performanter bei seltene Events z.B. fraud Baum mittels prior zu konfigurieren
# Insbesondere versuchen: 80/20, 90/10, 60/40, 50/50 => gute Heuristiken

# Modell mit 90%/10% prior
# with smaller complexity parameter to allow more complex trees
# for tuning complexity vs. pruning see Thernau 1997
model.tree.with.prior <- rpart(Creditability ~ ., 
             data=train,
             parms=list(prior=c(.8,.2)),
             cp=.0002)
plot(model.tree.with.prior);text(model.tree.with.prior);

test$tscore2 <- predict(model.tree.with.prior, type='prob', test)
pred2 <- prediction(test$tscore2[,2],test$Creditability)
perf2 <- performance(pred2,"tpr","fpr")


# Komplextität und out-of-sample error
printcp(model.tree)
printcp(model.tree.with.prior)
par(mfrow=c(2,1))
plotcp(model.tree)
plotcp(model.tree.with.prior)

# Modelle über ROC Kurve vergleichen
par(mfrow=c(1,1))
plot(perf1, col='red', lty=1, main='Tree vs Tree mit Prior');
plot(perf2, col='green', add=TRUE, lty=2);
legend(0.6,0.6,c('Einfacher Tree','Mit 90/10
                 prior'),col=c('red','green'),lwd=3)

print(model.tree)

# Konvertieren zu Regeln
list.rules.rpart(model.tree)
list.rules.rpart(model.tree.with.prior)


## Conditional Inference Trees
# erlauben Berechnung statistischer Signifikanz berechnen 
# auf Basis der Bonferroni-Metrik
# => signifikante Partionen werden angezeigt selektiert
cfit1<-ctree(Creditability~.,data=train)
plot(cfit1)

result.df <- as.data.frame(do.call("rbind", treeresponse(cfit1, newdata = test))) 
test$tscore3 <- result.df[,2]
pred3 <- prediction(test$tscore3,test$Creditability) 
perf3 <- performance(pred3,"tpr","fpr")
plot(perf1, col='red',lty=1, main='Tree vs Tree mit Prior vs Ctree'); 
plot(perf2, col='green',add=TRUE,lty=2);
plot(perf3, col='blue',add=TRUE,lty=3);
legend(0.6,0.6,c('Einfacher tree','90/10 prior','Conditional tree'),col=c('red','green','blue'),lwd=3)

## Random Forest
# ...last but not least...
model.rf <- randomForest(Creditability ~ . ,data=train,
                         importance=T,mtry=20,ntree=100, keep.forest=TRUE)
varImpPlot(model.rf)
test$tscore4 <- predict(model.rf, type='prob', test)[,2]
pred4 <- prediction(test$tscore4, test$Creditability) 
perf4 <- performance(pred4,"tpr","fpr")
plot(perf1,col='red',lty=1, main='rpart vs rf');
plot(perf4, col='blue',lty=2,add=TRUE);
legend(0.6,0.6,c('rpart','rf'),col=c('red','blue'),lwd=3)



## Bayesian Network 
# Hilft beim Erkennen von kausalen Beziehungen mittels gerichtetem Graph
# Behandelt Variablen wie r.v. und verwendet MCMC Methoden um die Beziehungen zu bestimemn
ksl<-train
ksl$Creditability <- as.numeric(train$Creditability)
ksl.nw<-network(ksl)
ksl.prior <- jointprior(ksl.nw)
ksl.nw <- learn(ksl.nw,ksl,ksl.prior)$nw
result <- heuristic(ksl.nw,ksl,ksl.prior,restart=1,degree=1,trace=TRUE)
thebest <- result$nw[[1]]
savenet(thebest, "ksl.net")
print(ksl.nw,condposterior=TRUE)





#####------------------------

#### Aus "Credit Scoring in R"
list.rules.rpart <- function(model)
{
  if (!inherits(model, "rpart")) stop("Not a legitimate rpart tree")
  #
  # Get some information.
  #
  frm     <- model$frame
  names   <- row.names(frm)
  ylevels <- attr(model, "ylevels")
  ds.size <- model$frame[1,]$n
  #
  # Print each leaf node as a rule.
  #
  for (i in 1:nrow(frm))
  {
    if (frm[i,1] == "<leaf>")
    {
      # The following [,5] is hardwired - needs work!
      cat("\n")
      cat(sprintf(" Rule number: %s ", names[i]))
      cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
                  ylevels[frm[i,]$yval], frm[i,]$n,
                  round(100*frm[i,]$n/ds.size), frm[i,]$yval2[,5]))
      pth <- path.rpart(model, nodes=as.numeric(names[i]),
                        print.it=FALSE)
      cat(sprintf("   %s\n", unlist(pth)[-1]), sep="")
    } }
}
