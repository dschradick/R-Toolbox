########## BOOSTING
# Alternativ mit => caret 
library(MASS)
library(randomForest)

#### Daten vorberiten
set.seed(1)
train <-  sample(1:nrow(Boston), nrow(Boston)/2)

#### Boosting
# Wichtigste Parameter:
# - distribution='gaussian'  => Regression
# - distribution='bernoulli' => Classification
# - n.trees = 5000           => 5000 Bäume
# - interaction.depth = 4    => Limitieren der Tiefe des Baumes auf 4
library(gbm)
set.seed(1)
boost.boston=gbm(medv ~ ., data=Boston[train,],
                 distribution='gaussian', 
                 n.trees=5000, interaction.depth=4)
# erzeut relative influence statisken & plot 
summary(boost.boston)

# !!! Partial dependence plots
# = Marginal Effect der variable auf die response 
#   nach integrieren über die anderen Variablen
par(mfrow=c(1,2))
plot(boost.boston,i="rm")    # => preise steigen mit rm
plot(boost.boston,i="lstat") # => preise fallen mit lstat

# Test-MSE
yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2) # ähnlich wie random-forest
# Mit anderem shrinkage-Wert 
# Default: lambda = 0.0001 => hier 0.2
boost.boston <- gbm(medv~., data=Boston[train,],
                    distribution="gaussian", n.trees=5000,
                    interaction.depth=4, shrinkage=0.2, verbose=F)
yhat.boost <- predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

