########## BAYES STATISTIK - MULTIPLE REGRESSION
library(tidyverse)
library(rjags)   
library(ggridges)
library(mosaic)     # railroad daten

########## Multiple Bayesian Regression
ggplot(RailTrail, aes(y = volume, x = hightemp, color = weekday)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# DEFINE the model    
rail_model_2 <- "model{
# Likelihood Modelle für Y[i]
for(i in 1:length(Y)) {
  Y[i] ~ dnorm(m[i], s^(-2))
  m[i] <- a + b[X[i]] + c * Z[i]
}

# Prior Modelle für a, b, c, s
a ~ dnorm(0, 200^(-2))
b[1] <- 0
b[2] ~ dnorm(0, 200^(-2))
c ~ dnorm(0, 20^(-2))
s ~ dunif(0, 200)
}"

rail_jags_2 <- jags.model(textConnection(rail_model_2), 
                          data = list(Y = RailTrail$volume, X = RailTrail$weekday, Z = RailTrail$hightemp), 
                          inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 10))

rail_sim_2 <- coda.samples(model = rail_jags_2, variable.names = c("a", "b", "c", "s"), n.iter = 10000)
rail_chains_2 <- data.frame(rail_sim_2[[1]])
plot(rail_sim_2)

### Inferenz
# Posterior mean regression models
ggplot(RailTrail, aes(y = volume, x = hightemp, color = weekday)) + 
  geom_point() + 
  geom_abline(intercept = mean(rail_chains_2$a), slope = mean(rail_chains_2$c), color = "red") + 
  geom_abline(intercept = mean(rail_chains_2$a) + mean(rail_chains_2$b.2.), slope = mean(rail_chains_2$c), color = "turquoise3")

# Posterior probability, dass typischen Volumen niedriger an Arbeitstagen 
mean(rail_chains_2$b.2. < 0)