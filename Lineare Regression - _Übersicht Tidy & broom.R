library(tidyverse)
library(modelr)
library(broom)

mod <- lm(mpg ~ ., mtcars)

#### VORBEREITUNGEN
### Resampling
## Train-Test-Split
rs         <- resample_partition(mtcars, c(test = 0.3, train = 0.7))
train_data <- as.data.frame(rs$train)
test_data  <- as.data.frame(rs$test)

## Cross-validation
cv <- crossv_kfold(mtcars, 5)
mods <- map(cv$train, ~ lm(mpg ~ wt, data = .))
map2_dbl(mods, cv$test, modelr::rsquare)

#### MODELL AUFRÄUMEN
mod %>% tidy()         # Informationen über die Koeffizienten
mod %>% glance()       # Goodness of fit 
mod %>% augment()      # Informationen über die Beobachtungen

#### METRIKEN
rmse(mod, mtcars)
rsquare(mod, mtcars)

#### MODELL BENUTZEN
mtcars %>% add_predictions(mod)
mtcars %>% add_residuals(mod)
data_grid(mtcars, wt = seq_range(wt, 10), cyl, vs)
mtcars_mod <- lm(mpg ~ wt + cyl + vs, data = mtcars)
# For continuous variables, seq_range is useful
data_grid(mtcars, wt = seq_range(wt, 10), cyl, vs) %>% add_predictions(mtcars_mod)

## HILFSFUNKTIONEN
model_matrix(mtcars,  mpg ~ disp)  # Design Matrix
# => gut zur Untersuchung wie Formel umgesetzt wird (z.B x^2 vs I(x^2))
## Erzeugungen von grids 
data_grid(mtcars,cyl)              # gleich gespaced und geordered
data_grid(mtcars,cyl,am)           # alle Kombinationen
grid1 %>% add_predictions(mod)     

## Mehrere Modelle 
gather_predictions()  # für mehrere Modelle fügt jede Prediction als Reihe hinzu
gather_residuals()
spread_predictions()
spread_residuals()

#### Funktionen
add_predictions(grid1,mod)      # fügt residuals hinzu
add_predictions    


## Transformationen
#  I() benutzen, wenn Transformation +, *, ^, - enthält
#  y ~ x + I(x^2)   =>  y = a_1 + a_2 * x + a_3 * x^2
#  y ~ x + x^2      =>  Interaktion mit sich sebls was dasselbse wie x ist
model_matrix(mtcars, mpg ~ disp + disp^2) 
model_matrix(mtcars, mpg ~ disp + I(disp^2))

View( sim1 %>%
  data_grid(x,y))


## Modelle vergleichen
# goodness of fit
fits <- list(
  fit1 = lm(hp ~ cyl, mtcars),
  fit2 = lm(hp ~ cyl + mpg, mtcars),
  fit3 = lm(hp ~ . , mtcars))
gof <- map_df(fits, glance, .id='model') %>% arrange(AIC)


## RESIDUEN VON MULTIPLE REGRESSION
# => wo ist noch ein Muster - Information - drin
mod <- lm(hp ~ cyl + mpg, mtcars) 
au <- augment(mod)
au %>%
  gather(x, val, -contains(".")) %>% 
  ggplot(aes(val, .resid)) +
    geom_point() +
    facet_wrap(~x, scales = "free") +
    labs(x = "Predictor value", y = "Residual") #+ 
#    theme(axis.text.x = element_blank(),
#          axis.ticks.x = element_blank())
