########### BAYES STATISTIK - ENSCHEIDUNGSANALYSE
library(tidyverse)
library(ggjoy)
library(ggExtra)
library(rjags)
library(BEST)


#### Ad-CTR Beispiel

## Angeblich 10% CTR auf Seite 
## Simulieren mit 100 ads

n_samples <- 100000
n_ads_shown <- 100
proportion_clicks <- 0.1
n_visitors <- rbinom(n_samples, size = n_ads_shown, 
                     prob = proportion_clicks)
hist(n_visitors) 
# => in ~90% der Fälle mehr als 5 Klicks

### Problem: 
#   Nicht sicher ob 10% immer stimmt
# Lösung:  
#   Anstatt einen Wert nun eine Menge von Werten, 
#   welche aus Wahrscheinlichkeitsverteilung kommen
#
# Update proportion_clicks
proportion_clicks <- runif(n = n_samples, min = 0.0, max = 0.2)
n_visitors <- rbinom(n = n_samples, size = n_ads_shown, prob = proportion_clicks)
hist(proportion_clicks)
hist(n_visitors) # => ~70% der Fälle mehr als 5 Klicks

#### Bayes Inferenz: Was ist wahrscheinlicher Parameter
# = konditionieren der Daten, um zu bestimmen mit 
#   welchen Parameterwerten wahrscheinlich erzeugt wurden
# 
## Nachdem tatsächliche Daten gesammelt wurden:
# Gemessene Daten: 5 Klicks

# Joint Probability betrachten
prior <- data.frame(proportion_clicks, n_visitors)
p <- ggplot(prior, aes(x=n_visitors,y=proportion_clicks)) + 
  geom_point(size=1, color='steelblue') 
ggMarginal(p,type = "histogram")


# Histogramm zeigen marginal distributions
# Nun conditioning auf 13 user
p <- ggplot(prior %>% filter(n_visitors==13), aes(n_visitors,proportion_clicks)) + 
  geom_point(size=1,color='steelblue') 
ggMarginal(p,type = "histogram")
# => proportion clicks scheint zwischen 7% und 19% zu liegen
p <- ggplot(prior %>% filter(n_visitors==5), aes(n_visitors,proportion_clicks)) + 
  geom_point(size=1,color='steelblue') 
ggMarginal(p,type = "histogram")
# => wäre 5 beobachtet worden, dann proportion clicks scheint zwischen 3% und 10% zu liegen

# Analog: wenn man weiss, dass CTR tatsächlich 10% ist, dann kann man darauf konditionieren
# und betrachten, wie dann die Verteilung der Visitors sein müsste
prior %>% filter(proportion_clicks >= 0.09, proportion_clicks <=0.11)
ggplot(prior %>% filter(proportion_clicks >= 0.095, proportion_clicks <=0.11), aes(n_visitors)) + 
  geom_histogram(color='steelblue')

### Update des Models 
posterior <- prior %>% filter(n_visitors == 13)
hist(prior$proportion_clicks)
hist(posterior$proportion_clicks)

### Wieviele visitors würde man bekommen, wenn man campaign nochmal durchführt
prior <- posterior
# n_visitors mit neuem sample ersetzen
n_samples <-  nrow(prior)
n_ads_shown <- 100
prior$n_visitors <- rbinom(n_samples, size = n_ads_shown, prob = prior$proportion_clicks)
hist(prior$n_visitors)
# Wahrscheinlichkeit mehr als 5 Klicks zu beobachen
sum(prior$n_visitors >= 5) / length(prior$n_visitors) 


#### Prior benutzen
# Zu integrierendes Wissen:
#    CTR ist meist ~5%, kann aber bis 2% runtergehen und auch mal 8% sein
# => durch beta mit shape1 = 5 und shape2 = 95
n_draws <- 100000
proportion_clicks <-rbeta(n_draws, shape1 = 5, shape2 = 95)
n_visitors <- rbinom(n_draws, size = n_ads_shown, prob = proportion_clicks)
prior <-data.frame(proportion_clicks, n_visitors)
posterior <- prior[prior$n_visitors == 13, ]

par(mfcol = c(2, 1))
hist(prior$proportion_clicks, 
     xlim = c(0, 0.25))
hist(posterior$proportion_clicks, 
     xlim = c(0, 0.25))


#### Gruppen vergleichen
# Idee: Verteilung für die wahrscheinliche Differenz in Proportion erstellen
# Wie: Subtrahieren der Posteriors voneinander (welche Vektoren von Samples sind)
# Binomial Model fitten mit:
# - Videos Ads Resultat: 13 / 100 Klicks
# - Text Ads Resultat:    6 / 100 Klicks
proportion_clicks <- runif(n_draws, min = 0.0, max = 0.2)
n_visitors <- rbinom(n = n_draws, size = n_ads_shown,  prob = proportion_clicks)
prior <- data.frame(proportion_clicks, n_visitors)

## Posteriors für video und text ads
posterior_video <- prior[prior$n_visitors == 13, ]
posterior_text <- prior[prior$n_visitors == 6, ]
hist(posterior_video$proportion_clicks, xlim = c(0, 0.25))
hist(posterior_text$proportion_clicks, xlim = c(0, 0.25))

## Berechnen der posterior Verteilung über 
## die wahrscheinliche Differenz in Proportion zwischen den Gruppen
posterior <- data.frame(
  video_prop = posterior_video$proportion_clicks[1:4000],
  text_prop  = posterior_text$proportion_click[1:4000])
posterior$prop_diff = posterior$video_prop - posterior$text_prop
hist(posterior$prop_diff)
summary(posterior$prop_diff)

## Berechnen der "am wahrscheinlichsten" Differenz
median(posterior$prop_diff)
## Berechnen des zugehörigen Credible Intervals


## Berechnen der Wahrscheinlichkeit, dass die Wahrscheinlichkeit
## dass die Proportion der Klicks für Video ads hüher als Text ads ist
sum(posterior$prop_diff > 0) / length(posterior$prop_diff)
# => Wahrscheinlihkeit, dass Video ads besser als Text ads = 95% 


### Entscheidungs-Analyse
# Durschnittlicher Umsatz pro Kunde: $2.53 o
# Kosten von Video-AD:$0.25
# Kosten Text-Ad: $0.05
# => was ist der Umsatz der beiden Kanäle
visitor_revenue <- 2.53
video_cost <- 0.25
text_cost <- 0.05

posterior$video_profit <- posterior$video_prop * visitor_revenue - video_cost
posterior$text_profit <- posterior$text_prop * visitor_revenue - text_cost
hist(posterior$video_profit)
hist(posterior$text_profit)
head(posterior)

## Welcher Kanal ist profitabler
# Bilden der Differenz der Profite: 
# Wenn differenz > 0, dann video_ads profitables
# Add the column posterior$profit_diff
posterior$profit_diff <- posterior$video_profit - posterior$text_profit
hist(posterior$profit_diff)
head(posterior)

# "best guess" für die Differenz im Profit
median(posterior$profit_diff)
# => ca. 3 cents weniger Profit bei video-ads

# Wahrscheinlichkeit, dass text ads besser sind als Video Ads
sum(posterior$profit_diff < 0) / length(posterior$profit_diff)
# => Obowhl Text Ads geringere CTR haben, sind sie  
#    durch niedrigere Kosten in 63% der Fälle profitabler!!



#### Ändern des unterliegenden statistischen Modells
# => nicht mehr CTR sondern 
#    visitors per day von Anzeige die ganzen Tag geschaltet
#   ==> Posion- statt Binomial-veteilt
# Beispiel: Ad auf Seite hat 18 visitors pro Tag gebracht
n_draws <- 100000
mean_clicks <- runif(n_draws, min = 0, max = 80)    # Prior: nun 0 bis 80
n_visitors <- rpois(n_draws, lambda = mean_clicks)  # jetzt rpois statt rbinom

prior <- data.frame(mean_clicks, n_visitors)  
posterior <- prior[prior$n_visitors == 19, ]        # Konditionierung auf 19 am Tag beobachtet

hist(prior$mean_clicks)
hist(posterior$mean_clicks)
# => wahrscheinlich zwischen 12 und 28
# => Um zu gucken ob es sich lohnt könnte man Enscheidungsanalyse machen


#### Bayes Theorem
### Simulation vs Berechung
##
## P(n_visitors = 13 | prob_success = 10%)
## Simulation
n_visitors <- rbinom(n = 100000, size = 100, prob = 0.1) sum(n_visitors == 13) / length(n_visitors)
hist(n_visitors)
## Berechnung
dbinom(13, size = 100, prob = 0.1)

## Berechnung der Verteilungen: 

# P(n_visitors | prob_success = 10%)
n_visitors = seq(0, 100, by = 1)
proportion_clicks <- 0.1
prob <- dbinom(n_visitors, size = n_ads_shown, prob = proportion_clicks)
plot(n_visitors,prob,type='h')

# P(prob_success | n_visitors = 13)
proportion_clicks <- seq(0, 1, by = 0.01)
n_visitors <- 13
prob <- dbinom(n_visitors, size = n_ads_shown, prob = proportion_clicks)
plot(proportion_clicks, prob, type = "h")
# Maximum likehood estimate von proportion_clicks ist 0,13


### Berechnung mit Density
## Die alte Simulations-Formulierung...
n_ads_shown <- 100
proportion_clicks <- 0.1
n_visitors <- rbinom(n = 99999, size = n_ads_shown, prob = proportion_clicks)
prob_13_visitors <- sum(n_visitors == 13) / length(n_visitors)
## ...wird zu:
prob_13_visitors <- dbinom(13, size=n_ads_shown, prob=proportion_clicks) 




### Komplette Bayes Inferenz durch Berechnung
n_ads_shown <- 100
n_visitors <- seq(0, 100, by = 1)
proportion_clicks <- seq(0, 1, by = 0.01)
parameters <- expand.grid(proportion_clicks = proportion_clicks, n_visitors = n_visitors)
parameters # Kombinationen von n_visitors und proportion_clicks
parameters$prior <- dunif(parameters$proportion_clicks, min = 0, max = 0.3)
# Anstatt: runif(n_samples, min = 0.0, max = 0.2)
# 5 oder 0 (weil kontinuierliche Verteilung)
parameters$likelihood <- dbinom(parameters$n_visitors, size = n_ads_shown, prob = parameters$proportion_clicks)
# Anstatt: n_visitors <- rbinom(n = n_samples, size = n_ads_shown, prob = proportion_clicks
# => likelihood der beobachteten Daten
parameters$probability <- parameters$likelihood * parameters$prior 
# ====> DER ZÄHLER IM BAYES THEOREM
# Produkt-regel der Warhscheinlichkeiten
# => Wahrscheinlichkeit von jeder Kombination von proportion_clicks & n_visitors
# Kombination ist die Wahrscheinlichkeit 
# - der Likelihood der Daten bei gegebenem Parameter Wert 
# - und der Wahrscheinlich dass der der Parameter den Wert annimmt
parameters$probability <- parameters$probability / sum(parameters$probability) 
# => DER NENNER IM BAYES THEOREM => Normalisierung
## ===> Resultat: Joint distribution
parameters

# Konditionieren
parameters %>% filter(n_visitors == 13) %>% select(probability)

## Visualisieren der Joint Distribution
ggplot(parameters %>% filter(probability > 0, n_visitors <= 30),
       aes(n_visitors,proportion_clicks,fill=probability)) + 
  geom_tile() + 
  scale_fill_gradientn(colours = rev(terrain.colors(15)))

## Konditionieren auf n_visitors==13  
ggplot(parameters %>% filter(n_visitors == 13), 
       aes(proportion_clicks,probability)) + 
  geom_bar(stat='identity') + 
  xlim(0,0.3)


### Vereinfachen => gleich auf n_visitors==13 konditionieren
# => joint distribution muss nicht extra gebildet werden
n_ads_shown <- 100
proportion_clicks <- seq(0, 1, by = 0.01)
n_visitors <- 6
parameters <- expand.grid(proportion_clicks = proportion_clicks)
parameters$prior <- dunif(parameters$proportion_clicks, min = 0, max = 0.2) 
parameters$likelihood <- dbinom(n_visitors, size = n_ads_shown, prob = parameters$proportion_clicks)
parameters$probability <- parameters$likelihood * parameters$prior
parameters$probability <- parameters$probability / sum(parameters$probability)
plot(parameters$proportion_clicks, parameters$probability, type = "h")



#### Mehrere Datenpunkte & mehrere Parameter
### Normal Verteilung
# => 2 Parameter zu schätzen

### Mehrere Datenpunkte
temp <- c(19, 23, 20, 17, 23)
likelihoods <- dnorm(x = temp, mean = 20, sd = 2)
# => wahrscheinlichkeiten jede Einzelne Temperatur zu beobachten
#    gegeben, dass mean=20 und sd=2 
#    => liklihood des datenpunktes
prod(likelihoods)
# Wahrscheinlichkeit den erste Punkt zusehen UND den zweiten UND ..
# Produktregel = likelihood aller datenpunkte zusammen 
# (wird sehr klein deshalb normalerweise log(like))


## Modell bauen
temp <- c(19, 23, 20, 17, 23)
mu <- seq(8, 30, by = 0.5)
sigma <- seq(0.1, 10, by = 0.3)
parameters <- expand.grid(mu = mu, sigma = sigma) 
parameters$mu_prior <- dnorm(parameters$mu, mean = 18, sd = 5) 
parameters$sigma_prior <- dunif(parameters$sigma, min = 0, max = 10) 
parameters$prior <- parameters$mu_prior * parameters$sigma_prior
head(parameters)
for(i in 1:nrow(parameters)) {
  likelihoods <- dnorm(temp, parameters$mu[i], parameters$sigma[i]) 
  # likelihood der Datenpunkte
  parameters$likelihood[i] <- prod(likelihoods)
  # => likelihoods aller datenpunkte zusammen
}
parameters$probability <- parameters$likelihood * parameters$prior 
parameters$probability <- parameters$probability / sum(parameters$probability)


#### Sampling von der Posterior um Größen zu bestimmen
sample_indices <- sample( nrow(parameters), size = 10000,
                          replace = TRUE, prob = parameters$probability)
head(sample_indices)
pars_sample <- parameters[sample_indices, c("mu", "sigma")]
hist(pars_sample$mu, 100, col = 'blue')
quantile(pars_sample$mu, c(0.025, 0.5, 0.975))


### Vorhersage
# also keine Aussage über die Parameter sondern über den Wert des generienden Modells
# => simulieren der daten mit basierend auf den Parametern des samples der posterior!!
#    (rnorm looped implizit über die Parameter-Werte)
pred_temp <- rnorm(10000, mean = pars_sample$mu, sd = pars_sample$sigma)
# Visualize pred_iq
hist(pred_temp)
# Wahrscheinlichkeit, dass mehr als 18 Grad
sum(pred_temp >= 18) / length(pred_temp)
# => 73% Wahrscheinlichkeit