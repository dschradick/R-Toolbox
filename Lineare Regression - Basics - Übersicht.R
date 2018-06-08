########## LINEARE REGRESSION - BASICS
library(tidyverse)
library(broom)
library(DAAG)


## Modell Erstellung
# ?forumla
# a:b = Interaction
# a*b = a + b + a:b
# I() = arithmetisch => I(a+b) = Summe von a und b
model <- lm(formula = mpg ~ I(disp * hp), data=mtcars)

## Zusammenfassung
summary(model)                  

## Diagnostik-Plots
layout(matrix(c(1,2,3,4),2,2))
plot(model)

## Stepwise Regression
model <- lm(formula = mpg ~ ., data=mtcars)
smaller_model <- step(model, direction='both')

## Model Fit
model_summary <- summary(model)
model_summary$r.squared           # R^2
model_summary$adj.r.squared       # R^2_adj
cv.lm(mtcars, model, m=3)         # Cross-Validation


## Modell-Informationen
coef(model)                     # Koeffizienten
confint(model, level=0.95)      # Konfidenzintervalle
fitted(model)                   # Vohergesagte Werte
residuals(model)                # Residuen
anova(model)                    # ANOVA Tabelle => Sum Sq
influence(model)                # Modell-Diagnostik
vcov(model)                     # Kovarianz-Matrix der Modell-Parameter

## Broom
model %>% glance()              # Modell Zusmmenfassung als frame
model %>% tidy()                # Koeffizienten + statistiken als frame
model %>% augment()             # Beobachtungen mit fitted values, residuals,...
# => gut um z.B. Outlier über cooks distance zu finden


### Vorhersage
new_observations = data.frame(disp = 160)
predict(model,newdata = new_observations)
predict(mmodelod) 


### Referenz-level setzen
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$cyl <- relevel(mtcars$cyl, ref = "4")
model <- lm(formula = mpg ~ disp + hp + cyl, data=mtcars)
