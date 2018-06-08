########## AB-TESTING - MULTIVARIATE TESTING
library(tidyverse)
library(lubridate)
library(broom)
library(powerMediation) # Sample Size Berechung für logistische Regression
library(pwr)            # Sample Size Berechung für t-test

viz_website_2018_05  <- read_csv("~/Documents/Data/viz_website_2018_05.csv") 

#### Multivariates Testen
# Beispiel: Zwei Wörter auf Homepage ändern
#
# Gesucht: Auswirkungen auf time_spent 

lm(time_spent_homepage_sec ~ word_one * word_two,
   data = viz_website_2018_05) %>% tidy() -> multivar_results
multivar_results

## Faktoren anpassen
multivar_results <- viz_website_2018_05 %>%
  mutate(word_one = factor(word_one, levels = c("tips", "tools"))) %>%
  mutate(word_two = factor(word_two, levels = c("better", "amazing"))) %>%
  lm(time_spent_homepage_sec ~ word_one * word_two,
     data = .) %>%
  tidy()
multivar_results

## Plotten des Mulivariaten Ergebnis
viz_website_2018_05_sum <- viz_website_2018_05 %>%
  group_by(word_one, word_two) %>%
  summarize(mean_time_spent_homepage_sec = mean(time_spent_homepage_sec))

ggplot(viz_website_2018_05_sum,
       aes(x = word_one,
           y = mean_time_spent_homepage_sec,
           fill = word_two)) +
  geom_bar(stat = "identity", position = "dodge")
# => Am höchsten für tools/amazing kombination


### Auswirkungen auf Clicks
viz_website_2018_05_sum <- viz_website_2018_05 %>%
  group_by(word_one, word_two) %>%
  summarize(like_conversion_rate = mean(clicked_like))

ggplot(viz_website_2018_05_sum,
       aes(x = word_one,
           y = like_conversion_rate,
           fill = word_two)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)
# => bei Clicks noch stärkere Interaktion als bei time_spent

# Multivariater Test für clicks
viz_website_2018_05_like_results <- viz_website_2018_05 %>%
  mutate(word_one = factor(word_one, levels = c("tips", "tools"))) %>%
  mutate(word_two = factor(word_two, levels = c("better", "amazing"))) %>%
  glm(clicked_like ~ word_one * word_two,
      family = "binomial",
      data = .) %>%
  tidy()

viz_website_2018_05_like_results
# Kein Effekt von word_two für Basline 'Tips'
# Aber Effekt von word_one für 'Tools'