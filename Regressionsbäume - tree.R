########## ENTSCHEIDUNGSBÄUME - TREE
library(MASS)
library(tree)

#### Daten erzeugen
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)

#### Entscheidungsbaum mit tree()
tree.boston <- tree(medv ~ ., Boston,subset=train)
summary(tree.boston)
# => Anm: nur 3 Variablen wurden zur Konstruktion des trees benutzt

## Visualisieren des resulierenden Baumes
plot(tree.boston)
text(tree.boston,pretty=0)

## Pruning 
# Zuerst bestimmen, ob pruning des trees die Performance verbessert
cv.boston <- cv.tree(tree.boston)
cv.boston
plot(cv.boston$size,cv.boston$dev,type='b')
# => komplexeste ist der beste, wenn doch pruning, dann so: 
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

## Vorhersage auf Test-Set
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
# Test-MSE
mean((yhat-boston.test)^2) # 25.05