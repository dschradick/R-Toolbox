########## MODEL INFERENCE - IMP, PDP, ALE
# BESSERE PLOTS + ander packages: http://rpubs.com/dnchari/explainable_machine_learning
library(iml)
library(randomForest)
library(breakDown)
library(ggthemes)
library(tidyverse)


#### Daten 
data("Boston", package  = "MASS")
head(Boston)

#### Zu erklärendes Modell bauen
set.seed(0)
rf = randomForest(medv ~ ., data = Boston, ntree = 50)

#### Basis für den Explainer
X = Boston[which(names(Boston) != "medv")]
predictor = Predictor$new(rf, data = X, y = Boston$medv)
# => predictor enhält model und daten


#### Permutation Importance
imp = FeatureImp$new(predictor, loss = "mae")
plot(imp)
imp$results


## PDP + ICE
pdp = FeatureEffect$new(predictor, method='pdp', feature = "lstat")
pdp$plot()

pdp_ice = FeatureEffect$new(predictor, method='pdp+ice', feature = "lstat")
pdp_ice$plot()

effect = FeatureEffects$new(predictor)
effect$plot(features = c("lstat", "age", "rm"))



## ALE
ale = FeatureEffect$new(predictor, method='ale', feature = "lstat")
ale$plot()

ale$set.feature("rm")
ale$plot()

## Overall Interaction
interact = Interaction$new(predictor)
plot(interact)

### 2-way Interaktionen
interact = Interaction$new(predictor, feature = "lstat")
plot(interact)


## Feature Effects für alle gleichzeitig
effs = FeatureEffects$new(predictor)
plot(effs)

## Global Surrogate Model
# => hier: tree surrogate
tree = TreeSurrogate$new(predictor, maxdepth = 2)
plot(tree)
head(tree$predict(Boston))

### Einzelne Vorhersagen Erklären
## LIME - Local surrogate
lime.explain = LocalModel$new(predictor, x.interest = X[1,])
lime.explain$results
plot(lime.explain)

lime.explain$explain(X[2,])
plot(lime.explain)


## Mittels Shapley Values
shapley = Shapley$new(predictor, x.interest = X[1,])
shapley$plot()

shapley$explain(x.interest = X[2,])
shapley$plot()

## Ergebnis als df
results = shapley$results
head(results)



predict.function = function(model, new_observation) {
  predict(model, new_observation)
}
predict.function(rf, X[1, ])

br = broken(model = rf, 
            new_observation = X[1, ], 
            data = X, 
            baseline = "Intercept", 
            predict.function = predict.function, 
            keep_distributions = TRUE)

data.frame(y = br$contribution,
           x = br$variable) %>%
  ggplot(aes(x = reorder(x,y,FUN =desc), y = y)) +
  geom_bar(stat = "identity", fill = "#1F77B4", alpha = 0.8) +
  coord_flip()


ggplot(mtcars,aes(x=hp,y=mpg,color=as_factor(cyl))) + 
  geom_point() + 
  #geom_smooth(method = 'lm') +
  scale_color_tableau()
  
  

