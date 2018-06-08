########## MODELL EVALUATION
# Drei grundsätzliche Ansätze:
# 1. Validation-set
# 2. LOOCV 
# 3. k-fold Cross-Validation
library(ISLR) # datasets
library(dplyr)
library(boot) # cv.glm


## Valdiation-Set Ansatz
attach(Auto)
set.seed(1)
number_of_observations = nrow(Auto)
train = sample(number_of_observations,number_of_observations/2) # Indizes erzeugen
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit,Auto))[-train]^2)


## LOOCV Ansatz
glm.fit <- glm(mpg ~ horsepower, data = Auto) # kein family-arg => wie lm
coef(glm.fit)
cv.err <- cv.glm(Auto, glm.fit) # kein k => LOOCV
cv.err$delta # Erste = CV estimate, Zweite = Adjusted cross-validation estimate (Bias, wenn nicht LOOCV)


## K-Fold Cross-Validation Ansatz
cv.err <- cv.glm(Auto, glm.fit, K=2) # => 5-folds
cv.err$delta # Erste = CV estimate, Zweite = Adjusted cross-validation estimate (Bias, wenn nicht LOOCV)

# Dann performanter meherer Modelle zu testen
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10






