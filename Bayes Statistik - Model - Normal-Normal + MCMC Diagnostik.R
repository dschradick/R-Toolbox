########## BAYES STATISTIK - NORMAL-NORMAL MODEL
library(tidyverse)
library(rjags)      # Just Another Gibbs Sampler
library(ggridges)
library(openintro)  # für gewichtsdaten bdims
library(mosaic)     # railroad daten


##### Beispiel: Wie wirkt sich Schlafentzug auf Reaktionszeit(ms) aus
#
# Y_i = Differenz der Reaktionszeit von Proband i
#
# Likelihood: 
#   Y_i ~ N(m,s^2) 
#     => 2 Parameter: m und s
#     => 2 Priors
# Priors: 
#   m ∼ N(50,25^2)   
#   s ~ Unif(0,200)

### 1. Schritt bei Bayes Analyse: Visualisieren der Prior Models 
# Samples aus den Priors nehmen
prior_m <- rnorm(n = 10000, mean = 50, sd = 25)    
prior_s <- runif(n = 10000, min = 0, max = 200)    
samples <- data.frame(prior_m, prior_s)
ggplot(samples, aes(x = prior_m)) + geom_density()
ggplot(samples, aes(x = prior_s)) + geom_density()


## Likelihood
## Daten betrachten
sleep_study <- read_csv('~/Documents/Data/sleepstudy.csv')
head(sleep_study)
sleep_study <- sleep_study %>% mutate(diff_3 = day_3 - day_0)    
ggplot(sleep_study, aes(x = diff_3)) + 
  geom_histogram(binwidth = 20, color = "white")

sleep_study %>% summarize(mean(diff_3), sd(diff_3))
# => Angenommen Daten wurden durch Y_i ~ N(m,s^2) erzeugt,
#    dann am wahrscheinlichsten durch zu sehen bei: 
#    m = 26.3; sd = 37.2

# Prior versus Likelihood
# Prior = vorhandene Informationen vor der Studie
# Likelihood = reflektiert die relative kompatibilität der verschiedenen m
#              mit den Daten der Studie
prior <- rnorm(n = 10000, mean = prior_m, sd = prior_s) # 50,25?
#prior <- rnorm(n = 10000, mean = 50, sd = prior_s)    
likelihood <- rnorm(n = 10000, mean = 26.3, sd = 37.2)
data <- data.frame(m = c(prior, likelihood), d = rep(c("prior","likelihood(data)"), each = 10000))
ggplot(data, aes(x = m, fill = d)) + geom_density(alpha = 0.5)


#### 2. Schritt Modellieren
### RJAGS
# 3 Schritte: define, compile, simulate
#
# 18 Datenpunkte für die Probanden
# RJAGS benutzt Precision statt Varianz 
# Precision = inverse Varianz # => N(a,b^(−1))

## 1. Definieren des Modells
sleep_model <- "model{
    for(i in 1:length(Y)) {     # Likelihood model für Y[i] => für jeden der 18 Probanden
      Y[i] ~ dnorm(m, s^(-2))   # s^2 => s^(-2)
    }
    m ~ dnorm(50, 25^(-2))      # Prior für m
    s ~ dunif(0, 200)           # Prior für s
}"
sleep_study
## 2. Kompilieren des Modells
sleep_jags <- jags.model(
  textConnection(sleep_model),
  data = list(Y = sleep_study$diff_3),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989)
)

## 3. Simulieren des posterior 
sleep_sim <- coda.samples(model = sleep_jags, 
                          variable.names = c("m", "s"), 
                          n.iter = 10000)
plot(sleep_sim, trace = FALSE)

summary(sleep_sim) # m = 29.29, s = 8.980
posterior_sample <- rnorm(n = 10000, mean =29.29, sd = 8.980) 
data <- data.frame(m = c(prior, likelihood,posterior_sample), d = rep(c("prior","likelihood(data)",'posterior'), each = 10000))
ggplot(data, aes(x = m, fill = d)) + geom_density(alpha = 0.5)


#### Markov Chain
head(sleep_sim)
sleep_chains <- data.frame(sleep_sim[[1]], iter = 1:10000)
head(sleep_chains)

# Die ersten 100 Iteration der m chain
ggplot(sleep_chains[1:100, ], aes(x = iter, y = m)) + geom_line()

## Trace Plots von m und s
# zur Darstellung des longitudinal behaviour
plot(sleep_sim, density = FALSE)
ggplot(sleep_chains, aes(x = iter, y = m)) + geom_line()


## Density der Chain
# => stellt die Approximation des posteriors dar
plot(sleep_sim, trace = FALSE)
ggplot(sleep_chains, aes(x = m)) + geom_density()

### Diagnostics der Chain / des angenäherten posteriors
# Ziel:    Stabilität der Kette
# Lösung:  mehr Iterationen
## 1. Trace plot: Stabil? 
## 2. Mehrere Ketten - hier 4
# haben verschiedene Verhalten => mehr Iterationen
sleep_jags_multi <- jags.model(textConnection(sleep_model), 
                               data = list(Y = sleep_study$diff_3), 
                               n.chains = 4)   
sleep_sim_multi <- coda.samples(model = sleep_jags_multi, variable.names = c("m", "s"), n.iter = 1000)
head(sleep_sim_multi)
plot(sleep_sim_multi, density = FALSE)
## 3. Standard Error
# Zu gross => mehr Iterationen
sleep_sim_1 <- coda.samples(model = sleep_jags, variable.names = c("m", "s"), n.iter = 1000)
summary(sleep_sim_1)
sleep_sim_2 <- coda.samples(model = sleep_jags, variable.names = c("m", "s"), n.iter = 10000)
summary(sleep_sim_2)


#### Reproduzierbarkeit
# Seed beim compilieren des Modells setzen
sleep_jags <- jags.model(textConnection(sleep_model), 
                         data = list(Y = sleep_study$diff_3), 
                         inits = list(.RNG.name = "base::Wichmann-Hill", 
                                      .RNG.seed = 1)) 



