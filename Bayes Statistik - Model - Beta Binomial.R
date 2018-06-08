########## BAYES STATISTIK - BETA-BINOMIAL MODEL
library(tidyverse)
library(ggridges)
#install.packages('mosaic')

##### BETA-BINOMIAL MODEL
# Beispiel: Politische Wahl
# Am wahrscheinlichsten: 45% der Stimmen - aber möglich von 30% bis 60&
#
# Erste Poll:   6/10        Stimmen sind für einen
# Zweiter Poll: 214 / 390   Stimmen sind für einen
# => Kombiniert: 220 / 400

#### Finden des Priors: Tuning des Beta Prior
# => anpassen der Parameter a und b, um geeigneten Prior zu finden
#
## Sampling von 10000 Werten aus Beta(45,55) prior
prior_A <- rbeta(n = 10000, shape1 = 45, shape2 = 55)
prior_sim <- data.frame(prior_A)
ggplot(prior_sim, aes(x = prior_A)) + geom_density()
## Mehrere vergleichen
prior_B <- rbeta(n = 10000, shape1 = 1, shape2 = 1)     # Beta(1,1) prior
prior_C <- rbeta(n = 10000, shape1 = 100, shape2 = 100) # Beta(100,100) prior
prior_sim <- data.frame(samples = c(prior_A, prior_B, prior_C), priors = rep(c("A","B","C"), each = 10000))
ggplot(prior_sim, aes(x = samples, fill = priors)) + geom_density(alpha = 0.5)



#### Likelihood: Simulieren der Abhängigkeit X von p
# Simulation von einem 1 poll Resultat für jedes p im grid
p_grid <- seq(from = 0, to = 1, length.out = 1000)
# vector version zieht n=1000 samples von Bin(size=10, prob) 
poll_result <- rbinom(n = 1000, size = 10, prob = p_grid)    
likelihood_sim <- data.frame(p_grid, poll_result)    
ggplot(likelihood_sim, aes(x = p_grid, y = poll_result, 
                           group = poll_result, fill = poll_result == 6)) + 
  geom_density_ridges()


### RJAGS
# 3 Schritte: define, compile, simulate

## 1. Definieren des Modells
# X ~ Bin(n,p)
# p ~ Beta(a,b)
vote_model <- "model{
  X ~ dbin(p, n)     # Likelihood model für X 
  p ~ dbeta(a, b)    # Prior model für p     
}"


## 2. Kompilieren des Modells
## Andere Priors: z.B. uninformative: data = list(a = 1, b = 1, X = 6, n = 10),
vote_jags <- jags.model(textConnection(vote_model), 
                        data = list(a = 45, b = 55, X = 6, n = 10),
                        inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 100))

## 3. Simulieren des posteriors 
# durch entnehmen von 10.000 samples aus der posterior
vote_sim <- coda.samples(model = vote_jags, 
                         variable.names = c("p"), 
                         n.iter = 10000)
plot(vote_sim, trace = FALSE)

