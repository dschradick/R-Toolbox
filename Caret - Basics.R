########## CARET
library(caret)
library(doMC)

#### Modell trainieren
# method = trees: rpart,ctree,rf,extraTrees
#          linear: glm,lasso,enet,logreg,polr,lda,pcr
#          svm: svmLinear,svmPoly,svmRadial
#          boosting (trees): adaboost,xgbTree
#          other: nb,nnet,knn
# Anmerkungen: 
# - na.action = na.pass => falls missing data
# - train generiert immer Dummy-Variablen
# - Classification => Response muss factor sein
set.seed(23)
train(hp ~ ., method = "rf", importance = TRUE, data = mtcars)


#### Preprocessing
# Anwenden von Transformationen, filters, etc auf Prädikatoren
# train(, preProc = c("method1", "method2"))
#
# Imputation:                knnImpute, bagImpute, medianImpute
# Normalisierung:            center, scaled, range
# Transformation:            BoxCox, YeoJohnson, expoTrans
# Erweiterte Transformation: pca, ica, spatialSign
train(hp ~ ., data = mtcars, preProc = c("medianImpute","center"))


#### Resampling
# trainControl(method = <method>, <options>)
#
# cv:  K-fold cross-validation (number = #folds).
# repeatedcv: repeated cross-validation; number, repeats = #repeats
# boot: bootstrap - number = sets the iterations).
# LOO: leave-one-out cross-validation
# timeslice: time-series
control <- trainControl(method="repeatedcv", number=10, repeats=3)
train(hp ~ ., data = mtcars, trControl=control)


#### Grid Search
# durch tuneGrid
# tuneLength = wieviele werte pro tuning parameter evalulieren 
# alternativ: trControl=trainControl(search = "random")
grid <- expand.grid(alpha = c(0.01,0.05, 0.1, 0.5, 0.9), lambda = c(0.001, 0.01))
glm.model <- train(hp ~ ., data = mtcars, method = "glmnet",tuneGrid = grid)
print(glm.model)
plot(glm.model)


#### Subsampling
# Class-Imbalance => vor dem fitten subsampling, um Daten zu balancieren 
# Sampling Methoden: 
# "down", "up", "smote", or "rose"
train(cyl ~ ., data = mtcars, trControl=trainControl(sampling = "down"))


#### Mehrere Kerne verwenden
# Windows: library(doParallel); cl <- makeCluster(2); registerDoParallel(cl)
registerDoMC(cores=4)
system.time({
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  train(hp ~ ., data = mtcars, trControl=control)
})

# RMSE() , R2() , caret::RMSE(pred = fit$finalModel$fitted.values, obs = Yvec)