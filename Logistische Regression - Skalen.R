########## LOGISTISCHE REGRESSION - SKALEN
library(dplyr)
library(ggplot2)
library(broom)
library(plotly)
library(modelr)
library(RColorBrewer)
library(plotly)
library(Stat2Data)
library(openintro)
data("MedGPA")

#### Daten vorbereiten
MedGPA$bin = cut(MedGPA$GPA,breaks = c(2.72,3.3,3.44,3.58,3.7,3.87,3.97)) 
MedGPA_binned <- MedGPA %>%
  group_by(bin) %>%
  summarize(mean_GPA = mean(GPA), acceptance_rate = mean(Acceptance)) %>%
  filter(acceptance_rate > 0)
MedGPA_binned[1,3] = 0.200
### Modell erstellen
mod <- glm(formula = Acceptance ~ GPA, family = binomial, data = MedGPA)


#### 3 Skalen Ansatz
#
# 1. PROBABILITY 
# yhat = exp(beta0hat + beta1hat * x) / (1 + exp(beta0hat + beta1hat * x))
# Einfach Wert zu interpretieren, 
# ABER da nicht linear: NICHT Veränderung beschreibbar wie..
#    "der anstieg einer unit von x ist assoziert mit..."  => geht nicht
#
# 2. ODDS SCALE
# Um das Problem von Probabilty anzugehen => Veränderung der Scala der y-Achse
# odds(yhat) = yhat / (1 - yhat) = exp(beta0hat + beta1hat * x)
# odds = verhältnis davon wie oft es passiert dazu wie oft es nicht passiert
#
# 3. LOG-ODDS
# logit(yhat) = ln(yhat / (1 - yhat)) = beta0hat + beta1hat * x
# Vorteil: logistisches regressionsmodell kann als Gerade dargestellt werden
# Problem: Fehlende Interpretierbarkeit
# ===> Jede Skala hat Vor- und Nachteile 


#### Probability Scale
# Skala: einfach zu interpretieren
# Logistische Funktion: nicht-linear => schwer zu interpretieren
# Koeffzienten nicht interpretierbar, da Funktion nicht linear
#
## y_hat hinzufügen
MedGPA_plus <- mod %>%
  augment(type.predict = "response") %>%
  mutate(y_hat = .fitted)
## Visualieren
ggplot(data = MedGPA_binned, aes(x = mean_GPA, y = acceptance_rate)) + 
  geom_point() + 
  geom_line() +
  geom_line(data = MedGPA_plus, aes(x = GPA, y = y_hat), color = "red")


#### Odds-Scale 
# Skala:                 schwere zu interpetieren => zwischen Probabiltity und Log-Odds 
# Logistische Funktion:  exponentiell - schwere zu interpretieren
# Koeffzienten:          interpretierbar durch bilden des Verhältnis der Odds
#                        wenn die unabhängige Variable um eine Unit ansteigt
#                        OR = odds(y|x+1) / odds(y|x) 
#                           = exp(beta0 + beta1 * (x + 1)) / exp(beta0 + beta1 * x)
#                           = exp(beta1)
#                        => Ungleich 1? Größer => odds steigen, Kleiner => odds fallen
# 
#                        Bsp: beta1 = 0.94 => 
#                        Jeder Anstieg der Variable um 1 Einheit ist verbunden 
#                        mit einem sinken der Odds un 6% der Zielvariable 
#
# Odds für bins berechnen und plotten
MedGPA_binned <- MedGPA_binned %>%
  mutate(odds = acceptance_rate / (1 - acceptance_rate))
# Odds für Observatioins berechen und plotten
MedGPA_plus <- MedGPA_plus %>%
  mutate(odds_hat = .fitted / (1 - .fitted))
# Plotten
ggplot(data = MedGPA_binned, aes(x = mean_GPA, y = odds)) + 
  geom_point() + 
  geom_line()  + 
  geom_line(data = MedGPA_plus, aes(x = GPA, y = odds_hat), color = "red")


#### Log Odds Scale
# Skala               :  nicht interpretierbar
# Logistische Funktion:  linear => einfach zu interpretieren
# Koeffizienen:       :  nicht interpretierbar, da Skala nicht interpretierbar
MedGPA_binned <- MedGPA_binned %>%
  mutate(log_odds = log(acceptance_rate / (1 - acceptance_rate)))
# Odds für Observatioins berechen und plotten
MedGPA_plus <- MedGPA_plus %>%
  mutate(log_odds_hat = log(.fitted / (1 - .fitted)))
# Plotten
ggplot(data = MedGPA_binned, aes(x = mean_GPA, y = log_odds)) + 
  geom_point() + 
  geom_line()  +
  geom_line(data = MedGPA_plus, aes(x = GPA, y = log_odds_hat), color = "red")


## Interpretation von Koeffizienten
# Probabilty: Koeffzienten nicht interpretierbar, da Funktion nicht linear
# Log-Odds:   Koeffizienten nicht interpretierbar, da Skala nicht interpretierbar
# Odds:       Koeffzienten interpretieren durch bilden des Verhältnis der Odds
#             wenn die unabhängige Variable um eine Unit ansteigt
#             OR = odds(y|x+1) / odds(y|x) 
#                = exp(beta0 + beta1 * (x + 1)) / exp(beta0 + beta1 * x)
#                = exp(beta1)
#             => Ungleich 1? Größer => odds steigen, Kleiner => odds fallen
# 
#             Bsp: beta1 = 0.94 => 
#             Jeder Anstieg der Variable um 1 Einheit ist verbunden 
#             mit einem sinken der Odds un 6% der Zielvariable 

