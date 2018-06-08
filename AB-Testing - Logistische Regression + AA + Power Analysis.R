########## AB-TESTING - BASICS
library(tidyverse)
library(lubridate)
library(broom)
library(powerMediation) # Sample Size Berechung für logistische Regression
library(pwr)            # Sample Size Berechung für t-test

  
#### Daten einlesen
click_data           <- read_csv("~/Documents/Data/click_data.csv")       # Baseline Daten
experiment_data      <- read_csv("~/Documents/Data/experiment_data.csv")  # Experiment 
viz_website_2017     <- read_csv("~/Documents/Data/viz_website_2017.csv") 
viz_website_2018_01  <- read_csv("~/Documents/Data/viz_website_2018_01.csv") 
viz_website_2018_04  <- read_csv("~/Documents/Data/viz_website_2018_04.csv") 
viz_website_2018_05  <- read_csv("~/Documents/Data/viz_website_2018_05.csv") 


#### Kurz EDA
click_data_sum <- click_data %>%
  group_by(month = month(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

ggplot(click_data_sum, aes(x = month, y = conversion_rate)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) 
# => seasonality



#### Power Analysis (für Proportion)
# CTR Daten => logistische Regression benutzen
# p2 = Erwartungswert für die Testcondition 
#    = Wahrscheinlichkeit, dass X = 1
# B  = proportion des Datensets mit X = 1
SSizeLogisticBin(p1 = 0.54,  # gemessene Daten (im Januar)
                 p2 = 0.64,  # => +10% Unterschied
                 B = 0.5,    # 50% im Datenset haben X=1
                 alpha = 0.05, power = 0.8) -> total_sample_size
total_sample_size       # insgesamt
total_sample_size / 2   # pro Gruppe



#### Analyse
## Zusammenfassung Control vs Treatment
experiment_data %>%
  group_by(condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

## Control vs Treatment visualisieren
experiment_data_sum <- experiment_data %>% 
  group_by(visit_date, condition) %>% 
  summarize(conversion_rate = mean(clicked_adopt_today))

ggplot(experiment_data_sum,
       aes(x = visit_date, y = conversion_rate, 
           color = condition, group = condition)) +
  geom_point() + 
  geom_line() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) 

## Statistischer Test
mod <- glm(clicked_adopt_today ~ condition, 
    family = "binomial",
    data = experiment_data) 

mod %>% tidy()
mod %>% glance()



#### A/A Test analysieren
# Überprüfen, ob Zuordnung wirklich random
viz_website_2018_01_sum <- 
  viz_website_2018_01 %>%
  group_by(condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))

viz_website_2018_01_sum

ggplot(viz_website_2018_01_sum,
       aes(x = condition, y = like_conversion_rate)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

aa_experiment_results <- glm(clicked_like ~ condition,
                             family = "binomial",
                             data = viz_website_2018_01) %>% tidy()
aa_experiment_results

# Alternativ
mod <- lm(time_spent_homepage_sec ~ condition,data = viz_website_2018_01) 

mod %>% summary()
mod %>% glance()
mod %>% tidy()



#### Power Analysis (für kontinuierliche abhängige Variable)
sample_size <- pwr.t.test(d = 0.3, sig.level = 0.05, power = 0.8)
sample_size


## Binäre response => Logistische Regression
ab_experiment_results <- glm(clicked_like ~ condition,
                             family = "binomial",
                             data = viz_website_2018_04) %>% tidy()
ab_experiment_results

##  Kontinuierliche Response => t-test oder Lineare Regression
ab_experiment_results <- t.test(time_spent_homepage_sec ~ condition,
                                data = viz_website_2018_04)
ab_experiment_results


