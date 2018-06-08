########## CARET REGRESSION
# Vollständige Regressionsmodell-Entwicklung mit Caret
# Ziel: gutes Prediction Model => nicht: erklärendes Modell
install.packages('mlbench')
library(mlbench)
library(caret)
library(corrplot)
library(factoextra)


#### Daten laden & vorbereiten
data(BostonHousing, package='mlbench')
set.seed(0)
split_index <- createDataPartition(BostonHousing$medv, p=0.80, list=FALSE)
# 20% als validation set
test_set <- BostonHousing[-split_index,]
# 80%
train_set <- BostonHousing[split_index,]


#### EDA

### Übersicht
## Dimensionen der Trainingsdaten
dim(train_set)
## Datentypen anzeigen
sapply(train_set, class)
## Verteilungen textuell betrachten
summary(train_set)
train_set[,4] <- as.numeric(as.character(train_set[,4]))
## Korrelationen
cor(train_set[,1:13])


### Univariate Verteilungen der Variablen 
# Als Histogramm
par(mfrow=c(2,7))
for(i in 1:ncol(train_set)) {hist(train_set[,i], main=names(train_set)[i])}
# Als Densityplot
for(i in 1:ncol(train_set)) {plot(density(train_set[,i]), main=names(train_set)[i])}
# Als Boxplot
for(i in 1:ncol(train_set)) {boxplot(train_set[,i], main=names(train_set)[i])}

## Bivariate & Multivariate Visualisierungen
# Scatterplot matrix anzeigen
pairs(train_set[,1:13])

# Einfacher Korrelationsplot
par(mfrow=c(1,1))
correlations <- cor(train_set[,1:13])
corrplot(correlations, method="circle")

# PCA
pca <- prcomp(scale(train_set[1:13]), center=TRUE, scale=TRUE)
fviz_pca_var(pca, col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


#### Spotchecking 
# Welcher Algorithmus ist für das gegebenen am besten geeignet? 
# => bester wird ausgewählt und später getuned
# => alternativ sogar mehrere und dann danach ensemble

## Jeder Algorithmus wird 10-fold Cross-Validation getestet
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# Regression => RMSE 
metric <- "RMSE"

set.seed(0) ; fit.lm <- train(medv~., data=train_set, method="lm", metric=metric, preProc=c("center", "scale"), trControl=control)
set.seed(0) ; fit.glm <- train(medv~., data=train_set, method="glm", metric=metric, preProc=c("center", "scale"), trControl=control)
set.seed(0) ; fit.glmnet <- train(medv~., data=train_set, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
set.seed(0) ; fit.svm <- train(medv~., data=train_set, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control)
set.seed(0) ; grid <- expand.grid(.cp=c(0, 0.05, 0.1))
fit.cart <- train(medv~., data=train_set, method="rpart", metric=metric, tuneGrid=grid, preProc=c("center", "scale"), trControl=control)
set.seed(0) ; fit.knn <- train(medv~., data=train_set, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)

## Performance der Algorithmen vergleichen
results <- resamples(list(LM=fit.lm, GLM=fit.glm, GLMNET=fit.glmnet, SVM=fit.svm, CART=fit.cart, KNN=fit.knn))
summary(results)
dotplot(results)


#### Feature Selection 
# Messen der Performance der Algorithmen, wenn korrelierte Features entfernt

# Finden von korrelierten Features
set.seed(0)
cutoff <- 0.70
correlations <- cor(train_set[,1:13])
highlyCorrelated <- findCorrelation(correlations, cutoff=cutoff)
for (value in highlyCorrelated) {
	print(names(train_set)[value])
}
# Neuen Teildatensatz erstellen von Trainingsdatensatz
# => ohne die korrelierten Features
train_features <- train_set[,-highlyCorrelated]
dim(train_features)

# Wieder 10-fold Cross-Validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
set.seed(0) ; fit.lm <- train(medv~., data=train_features, method="lm", metric=metric, preProc=c("center", "scale"), trControl=control)
set.seed(0) ; fit.glm <- train(medv~., data=train_features, method="glm", metric=metric, preProc=c("center", "scale"), trControl=control)
set.seed(0) ; fit.glmnet <- train(medv~., data=train_features, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
set.seed(0) ; fit.svm <- train(medv~., data=train_features, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control)
set.seed(0) ; grid <- expand.grid(.cp=c(0, 0.05, 0.1))
fit.cart <- train(medv~., data=train_features, method="rpart", metric=metric, tuneGrid=grid, preProc=c("center", "scale"), trControl=control)
set.seed(7) ; fit.knn <- train(medv~., data=train_features, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)

# Vergleichen der Algorithmen mit reduzierten Features
feature_results <- resamples(list(LM=fit.lm, GLM=fit.glm, GLMNET=fit.glmnet, SVM=fit.svm, CART=fit.cart, KNN=fit.knn))
summary(feature_results)
dotplot(feature_results)

## Algorithnmen nochmal vergleichen wie vorher - jetzt mit Box-Cox Transform - z.b.
fit.lm <- train(medv~., data=train_set, method="lm", metric=metric, preProc=c("center", "scale", "BoxCox"), trControl=control)
#...

#### Hyperparameter tuning

# Parameter angucken
print(fit.svm)

# Grid-Searching der Parametersigma und C
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(0)
grid <- expand.grid(
  .sigma = c(0.025, 0.05, 0.1, 0.15), 
  .C = seq(1, 10, by=1)
)
fit.svm <- train(medv~., data=train_set, method="svmRadial", metric=metric, tuneGrid=grid, preProc=c("BoxCox"), trControl=control)
print(fit.svm)
plot(fit.svm)


### Ensemble Methoden ausprobieren
# rf,gbm,....
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(0) ; fit.rf <- train(medv~., data=train_set, method="rf", metric=metric, preProc=c("BoxCox"), trControl=control)
set.seed(0) ; fit.gbm <- train(medv~., data=train_set, method="gbm", metric=metric, preProc=c("BoxCox"), trControl=control, verbose=FALSE)
set.seed(0) ; fit.cubist <- train(medv~., data=train_set, method="cubist", metric=metric, preProc=c("BoxCox"), trControl=control)
ensemble_results <- resamples(list(RF=fit.rf, GBM=fit.gbm, CUBIST=fit.cubist))
summary(ensemble_results)
dotplot(ensemble_results)


### Hyperparameter Tuning Cubist
# Parameter vom fit anschauen
print(fit.cubist)

# Grid-search
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(0)
grid <- expand.grid(.committees=seq(15, 25, by=1), .neighbors=c(3, 5, 7))
tune.cubist <- train(medv~., data=train_set, method="cubist", metric=metric, preProc=c("BoxCox"), tuneGrid=grid, trControl=control)
print(tune.cubist)
plot(tune.cubist)


### Model finalisieren

# Trainings-Daten transformation
set.seed(7)
x <- train_set[,1:13]
y <- train_set[,14]
preprocessParams <- preProcess(x, method=c("BoxCox"))
trans_x <- predict(preprocessParams, x)
# Endgültiges Modell transformieren
finalModel <- cubist(x=trans_x, y=y, committees=18)
summary(finalModel)

# Test-Daten transformieren
set.seed(7)
val_x <- test_set[,1:13]
trans_val_x <- predict(preprocessParams, val_x)
val_y <- test_set[,14]
# Predictions mit dem endgültigen Modell auf dem Test-Set
predictions <- predict(finalModel, newdata=trans_val_x, neighbors=3)
# RMSE berechnen
rmse <- RMSE(predictions, val_y)
r2 <- R2(predictions, val_y)
print(rmse)

