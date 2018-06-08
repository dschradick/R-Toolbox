########### BAYES - BASICS
library(tidyverse)
library(ggjoy)
library(ggExtra)
library(rjags)
library(BEST)

source('~/Documents/GIT/r_work/Bayes Statistik - Helfer.R')

#### Einfaches Beispiel
data = c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)

## Posterior betrachten
## Sampling von der Verteilung
posterior <- prop_model(data)
head(posterior)

# Posterior als Histogramm
data = c(1, 0, 0, 1, 0, 0,
         0, 0, 0, 0, 0, 0, 0)
posterior <- prop_model(data)
hist(posterior, breaks = 30, xlim = c(0, 1), col = "palegreen4")

## Schätzungen auf Basis des Posterior
# Punktschätzung für den Median
median(posterior)
# Berechnung des 95% Credibile Intervals
quantile(posterior, c(0.05, 0.95))


## Beispiel: 
# Vergleich mit Droge aus anderem Labor, welche angeblich 7% der Patienten heilt
sum(posterior > 0.07) / length(posterior)
# => 92% warhscheinlich, dass unser Medikament besser
hist(posterior, breaks=30, xlim=c(0,1), col='palegreen4')

# Resultat
# Bei den gegebenen Daten von 2 geheilten und 11 rückfälligen Patienten
# besagt das Bayes Modell, dass 
# - eine 90% Wahrscheinlichkeit besteht, dass
# - das Medikament zwischen 6% und 39% der behandelten Personen heilt.
# Desweiteren besteht eine 93% Wahrscheinlichkeit, dass das
# dass das Medikament die Krankheit mit einer höheren Rate 
# behandelt als die derzeitigen.




#### BEST 
# Bayesian Estimation Supersedes the t-test
# verwendet t-verteilung statt normal verteilung
iq_brains <- c(44, 52, 42, 66, 53, 42, 55, 57, 56, 51)
iq_regular <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 150) # 150 ist outlier

mean(iq_brains) - mean(iq_regular)

best_posterior <- BESTmcmc(iq_brains, iq_regular)
best_posterior
plot(best_posterior)


