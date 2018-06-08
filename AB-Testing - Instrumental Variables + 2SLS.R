library(tidyverse)
library(broom)


#### Frage: 
## 
##  "Beeinflusst eine längere Ladezeit, eine gegebene Engagement-Metrik "KPI"
##

#### Problem: Konfundierung
# Confounder: Usertype = gamer vs non-gamer
# => beeinflusst die 1. Gerätetyp = Ladezeit 2. Inhärentes Engagment für hardcore Spiele
# 
## Illustration
# Wenn Gamer => 1. schnelleres tablet = schneller Ladezeit
#            => 2. bereit für hardcore spiel zu warten 
# 
# ==> einfacher Verleich (z.B. Scatterplot, Mittelwerte in bins,...) 
#     von Ladezeit und Engagement Metrik funktioniert nicht
#
# Idee: Instrumentieren der Ladezeit durch A/B Test


#### Daten simulieren
set.seed(0)
n                      <- 10000
id                     <- 1:n

kpi_mean               <- 100
loadtime_mean          <- 50
extra_time             <- 10
effect_additional_sec  <- -0.1  # => effekt jeder zusätzlichen sekunde 

kpi            <- rnorm(n=n, sd=5, mean=kpi_mean)
loading_time   <- rnorm(n=n, sd=5, mean=loadtime_mean)

data <- tibble(
  id = id,
  kpi = kpi,
  loading_time = loading_time
)

#### Effekt von Ladezeit & AB-Test simulieren
data <- data %>%
  mutate(variant = if_else(id < n/2, 'a', 'b'),
         loading_time = if_else(variant == 'b',
                                loading_time + extra_time,
                                loading_time),
         kpi = kpi + (effect_additional_sec * loading_time))

head(data)


data %>% 
  group_by(variant) %>% 
  summarise(time_mean = mean(loading_time),
            kpi_mean = mean(kpi))

t.test(kpi ~ variant, data)

#### 2 Stage Least Square 

### First stage
mod_stage_1 <- lm(loading_time ~ variant, data=data)
mod_stage_1 %>% tidy()
# => Effekt von treatment b ist wie erwartet 10 sekunden längere Wartezeit

## Vorhersage mit Modell machen
data$load_time_predicted <- predict(mod_stage_1, data)

data$load_time_predicted %>% range()

### Second Stage
mod_stage_2 <- lm(kpi ~ load_time_predicted, data=data)
mod_stage_2 %>% tidy()
#
# 1 (Intercept)          99.9       0.554     180.   0.      
# 2 load_time_predicted  -0.0964    0.0100     -9.61 9.39e-22
#
# ==> Mittelwert für die KPI ~ 100
# ==> EFFEKT von jeder ZUSÄTZLICHEN SEKUNDE auf die KPI: ~ -0.1 
