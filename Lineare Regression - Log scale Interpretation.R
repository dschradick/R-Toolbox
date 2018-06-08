library(tidyverse)
library(broom)
options(scipen = 999)

# Normal: mpg vs hp
qplot(mtcars$mpg, mtcars$hp)

## Normal vs log-scale
mtcars %>%
  select(mpg, hp) %>%
  mutate(hp_log = log(hp)) %>%
  pivot_longer(cols=c('hp','hp_log'), 
               names_to = 'scale', values_to = 'hp') %>%
  ggplot(aes(x=hp, y=mpg)) + 
  facet_grid(~scale, scales = 'free_x') + 
  geom_point() + 
  geom_smooth(method='loess', se=F)


# Linear-Log Modell ----------------------------------------------------

## Modelle  
mod      <- lm(mpg ~ hp,      data=mtcars)
mod_log  <- lm(mpg ~ log(hp), data=mtcars)

(hp_est    <- coef(mod)['hp'])              # -0.0682
(hp_log_est <- coef(mod_log)['log(hp)'])    # -10.76418 
# Anstieg in hp = 1% resultiert 
# in anstieg von -10.76418 * 0.01 = -0.1071071 in mpg
# => NICHT:   1%-increase in hp ist assoziert mit coef     anstieg in Y
#    SNODERN: 1%-increase in hp ist assoziert mit coef/100 anstieg in Y

# Anstieg 1% anstieg = ansteig in log(hp) um log(1.01) ~ 0.01 
# => deshalb coef/100
# aufpassen: coef/100 ist absolut - ungleich coef% 

### Beispiel von 1% Anstieg
# beta_log(hp) = -10.76
# 1% Anstieg in X => coef * 0.01 anstieg in Y  -0.1071071
# Anstieg in Y = -0.1071071
y_1 <- predict(mod_log, newdata = data.frame(hp=c(200)))[[1]]
y_2 <- predict(mod_log, newdata = data.frame(hp=c(202)))[[1]] # anstieg um 1%
(diff <-  y_2 - y_1) 
# Anstieg in Y = -0.1071071

# 20% anstieg in X ist  mit coef * log(1.01) = coef * 0.01 anstieg assoziert
# 20% anstieg in X wäre mit coef * log(1.20) = coef * 0.18 anstieg assoziert

log(1.01)  #  = 0.009950331 = ~0.01 
# => Der Anstieg auf normaler Skala zm 1% 
#    entspricht dem addivten Anstieg um 0.01 auf log-skala
#    dh. steigt X um 1% wird auf log skala 0.01 dazu addiert, 
#        und das resultat mit coef multipliziert. 
#        => dh. Wert mit dem coef multipliert wird verändert sich
# => rechtfertigung für natural log interpretation als
#    "1% prozent in X für zu (absolutem) coef-unit anstieg in Y
mod_log



### Illustration
# X steigt um 1% = X * 1.01 => dann steigt der log um log(1.01) ~ 0.01 = 1%
# X <- 6
## Wert & Prozentanstieg auf normaler Skala
6 + 0.06             # Steigung um 1% = 6.6                
## Wert und Steigerung auf Log-Skala
log(6)               # Wert log scala                 = 1.79175
log(1.01)            # Anstieg in log-scala           ~ 0.01
                     # Anstieg von 6 auf 6.6 mutltipliktiv ausgedrückt = 1.01
## Addieren, weil auf normaler skala steigt = additiv (X steigt um 1% nicht log(X))
log(6) + log(1.01)   # Steigerung des Log um log(1.01) = 1.80171
# = log(6)  + 0.01 
# = 1.79175 + 0.01  = 1.80171
log(6 * 1.01)        # log regel                     = 1.80171
log(6.06)            #                               = 1.80171
# Rücktransformation: Erhöhung für zu richtigem Anstieg auf normaler Skala
exp(1.80171)         # 6.6

# => 0.18 =  18 * coef unit increase in Y?
log(6) + log(1.18)


# 1% anstieg auf normaler skala ~ log(1.01) anstieg auf multiplikative log-scala 
# = log(X) + log(1.01) = log(X * 1.01)) 
#
# 6 * 1.01 = 6 + 0.06 = 6.06
# log(6)  + log(1.01) = log(6 * 1.01) = log(6.06) = 1.80171
# 1.79175 +   0.01    = 1.80171
# => additiver Anstieg um 0.01 auf logscala

## fox
# Increasing X by 1% is equivalent to multiplying it by 1.01, 
# which in turn implies that the log of X increases by log10(1.01) = 0.00432.



# Log-Linear Modell ------------------------------------------------------

##### Log Transformation von abhängiger Variable
mod_y_log <- lm(log(mpg) ~ hp, data=mtcars); summary(mod_y_log)
(coef_hp <- coef(mod_y_log)[['hp']])
(intercept <- coef(mod_y_log)[['(Intercept)']])

y_1 <- predict(mod_y_log, newdata = data.frame(hp=c(200)))[[1]]
y_2 <- predict(mod_y_log, newdata = data.frame(hp=c(201)))[[1]] # 1 UNIT anstieg(vs 1%) anstieg
## Interpretation
# 1-Unit increase in X ist assoziiert
# mit e^coef-prozent Anstieg in Y (auf normaler Skala)

## Multiplikativer Anstieg
exp(coef_hp)  # 0.9965771 = 0.003% pro unit 
# Check 
exp(y_1)                  # für hp=200 auf normaler skala   = 16.03414 
exp(y_1) * exp(coef_hp)   # Unit increase simulieren        = 15.97925
exp(y_2)                  # Modell                          = 15.97925 √

# 5 Unit increase => jede unit jweils einen 1% increase (multiplikativ) 
# => 5-mal anwenden
y_1 <- predict(mod_y_log, newdata = data.frame(hp=c(200)))[[1]]
y_2 <- predict(mod_y_log, newdata = data.frame(hp=c(205)))[[1]] 
exp(y_1) * (exp(coef_hp) ** 5) # 15.7616
exp(y_2)                       # 15.7616 √
