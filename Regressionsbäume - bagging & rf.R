########## BAGGING & RANDOM FOREST
# Alternativ mit => caret 
library(MASS)
library(randomForest)

#### Daten vorberiten
set.seed(1)
train <-  sample(1:nrow(Boston), nrow(Boston)/2)

#### Bagging
# mtry = 13 => ALLE Variablen sollen bei jedem Split in betracht gezogen werden 
# ===> dh. bagging wird vorgenommen
# ntree = 25 => anzahl der zu wachsenden Bäume
bag.boston <- randomForest(medv ~ ., data=Boston,
                           subset=train, mtry=13, importance=TRUE)
# Resultierendes Modell
bag.boston

# Vorhersage auf Test-Set
yhat.bag <- predict(bag.boston,newdata=Boston[-train,])
boston.test <- Boston[-train,"medv"]
plot(yhat.bag, boston.test)
abline(0,1)
# Test-MSE
mean((yhat.bag-boston.test)^2) 
# 13.33 => bereits die hälfte vom optimal gepruneten (normalen) tree

## Random Forest
# Analog zu bagging, nur dass kleinerer Wert von mtry verwendet wird
# Default: p/3 für regression trees und sqrt(p) für classification
# hier: 6
set.seed(1)
rf.boston <- randomForest(medv ~ ., data=Boston, 
                          subset=train, mtry=6, importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2) # 11.48

## Variable-Importance anzeigen
# IncMSE = verringerung der performance, 
#          wenn variable nicht im modell für out-of-sample predictions
# IncNodePurity = totale verringerung der node impurity, 
#                 welche durch splits durch diese Variable entsteht,
#                 gemittelt über alle Bäume
importance(rf.boston)
varImpPlot(rf.boston)
# => lstat am wichtigsten


