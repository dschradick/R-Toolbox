########## BAYES STATISTIK - KATEGORISCHE VARIABLEN
library(tidyverse)
library(rjags)   
library(ggridges)
library(mosaic)     # railroad daten

# Faktor-Variable: Weekday
head(RailTrail)
class(RailTrail$weekday)
ggplot(RailTrail, aes(x = volume, fill = weekday)) + geom_density(alpha = 0.5)

### Modell bauen
# 1. Definieren des Modells
# a beschreibt das typische Wochenend-Volumen
# b beschreibt den Kontrast zwischen Weekday und Weekend Volume
#
rail_model_1 <- "model{
# Likelihood Modelle für Y[i]
for(i in 1:length(Y)) {
  Y[i] ~ dnorm(m[i], s^(-2))
  m[i] <- a + b[X[i]]
}

# Prior Modelle für a, b, s
a ~ dnorm(400, 100^(-2))
b[1] <- 0
b[2] ~ dnorm(0, 200^(-2))
s ~ dunif(0, 200)
}"

# 2. Kompilierend es Modells
rail_jags_1 <- jags.model(
  textConnection(rail_model_1),
  data = list(Y = RailTrail$volume, X = RailTrail$weekday),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 10)
)

# 3. Simulieren des Posteriors
rail_sim_1 <- coda.samples(model = rail_jags_1, variable.names = c("a", "b", "s"), n.iter = 10000)
# Kette speichern und posterior plotten
rail_chains_1 <- data.frame(rail_sim_1[[1]])
plot(rail_sim_1)


### Inferenz
# Posterior probability, dass das typische Volumen 
# an Wochentagen geringer ist
mean(rail_chains_1$b.2. < 0)

# Konstruktion von Kette von Werten für typische Wochentag-Volumen
rail_chains_1 <- rail_chains_1 %>% mutate(weekday_mean = a + b.2.)
ggplot(rail_chains_1, aes(x = weekday_mean)) + geom_density()
# 95% credible interval 
quantile(rail_chains_1$weekday_mean, c(0.025, 0.975))

