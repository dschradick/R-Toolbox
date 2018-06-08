########## BAYES STATISTIK - POISSON REGRESSION
library(tidyverse)
library(rjags)   
library(ggridges)
library(mosaic)     # railroad daten

#### Poisson Regression
# 
# 1. Modell definieren
poisson_model <- "model{
# Likelihood Modell für Y[i]
for(i in 1:length(Y)) {
  Y[i] ~ dpois(l[i])
  log(l[i]) <- a + b[X[i]] + c * Z[i]
}

# Prior models für a, b, c
a ~ dnorm(0, 200^(-2))
b[1] <- 0
b[2] ~ dnorm(0, 2^(-2))
c ~ dnorm(0, 2^(-2))
}"

## 2. Modell kompilieren
poisson_jags <- jags.model(
  textConnection(poisson_model),
  data = list(Y = RailTrail$volume, X = RailTrail$weekday, Z = RailTrail$hightemp),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 10)
)

# 3. Simulieren des Posterior
poisson_sim <- coda.samples(model = poisson_jags, variable.names = c("a", "b", "c"), n.iter = 10000)
poisson_chains <- data.frame(poisson_sim[[1]])
plot(poisson_sim)


### Inferenz für Poisson Modell und Rate-Parameter
## Plotten des posterior mean regression models
# Trend ist Kurve (vs normale Regression)
ggplot(RailTrail, aes(x = hightemp, y = volume, color = weekday)) + 
  geom_point() + 
  stat_function(fun = function(x){exp(mean(poisson_chains$a) + mean(poisson_chains$c) * x)}, color = "red") + 
  stat_function(fun = function(x){exp(mean(poisson_chains$a) + mean(poisson_chains$b.2.) + mean(poisson_chains$c) * x)}, color = "turquoise3")

# Typisches Volumen an 80 Grad Wochenende / Arbeitstag
poisson_chains <- poisson_chains %>% 
  mutate(l_weekend = exp(a + c * 80)) %>% 
  mutate(l_weekday = exp(a + b.2. + c * 80))
# CIs
quantile(poisson_chains$l_weekend, c(0.025, 0.975))
quantile(poisson_chains$l_weekday, c(0.025, 0.975))


### Poisson posterior prediction
# Simulieren von weekend & weekday Vorhersagen für jedes Parameterset
poisson_chains <- poisson_chains %>% 
  mutate(Y_weekend = rpois(n = 10000, lambda = l_weekend)) %>% 
  mutate(Y_weekday = rpois(n = 10000, lambda = l_weekday))

#Density plot für posterior weekday Vorhersagen
ggplot(poisson_chains, aes(x = Y_weekday)) + 
  geom_density()

# Posterior probability, dass Arbeitstag-Volumen < 400
mean(poisson_chains$Y_weekday < 400)

