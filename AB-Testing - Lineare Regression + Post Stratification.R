########## AB-TESTING - POST STRATIFICATION
library(tidyverse)
library(lsr)

#### Zu simulierende Daten
## Baseline: 
# Power-User:  durchschnittlicher Umsatz = 100€
# Casual-User: durchschnittlicher Umsatz = 10€
#
## Treatment:
# Hat einen Effekt von 0€ / 12€ 
effect <- 0
effect <- 12
# => Auswirkung bei mit vs. ohne stratification vergleichen

 
#### Daten simulieren
set.seed(0)
revenue_power_users  <- rnorm(n=50, sd=20, mean=300)
revenue_casual_users <- rnorm(n=50, sd=3, mean=10)

revenue <- c(revenue_power_users[1:20],
             revenue_casual_users[1:30],
             revenue_power_users[21:50],
             revenue_casual_users[31:50])

user_type <- c(rep('power',20),
               rep('casual',30),
               rep('power',30),
               rep('casual',20))

treatment <- c(rep('A',50),
               rep('B',50))

ab_data <- tibble(
  revenue = revenue,
  user_type = user_type,
  treatment = treatment
)

## Treatment Effekt hinzufügen
ab_data <- ab_data %>%
  mutate(revenue = if_else(treatment == 'B',
                           revenue+effect,
                           revenue))

## Post-Stratification mit Linearer Regression
mod <- lm(revenue ~ treatment + user_type, ab_data)
summary(mod)
mod %>% tidy() # => effekt von user_type raussepariert


#### VS OHNE STRATIFICATION
## =... 
#lm(revenue ~ treatment - 1, ab_data) %>% summary()
## =... 
t.test(revenue ~ treatment, ab_data) 

# => berichtet irreführendes Ergebnis, wegen verhältnis von power zu casual users
#    (mehr power-user in treatment)
table(ab_data$treatment,ab_data$user_type)
#cohensD(revenue ~ treatment, ab_data)




