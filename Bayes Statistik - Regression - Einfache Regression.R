########## BAYES STATISTIK - EINFACHE REGRESSION
library(tidyverse)
library(rjags)   
library(ggridges)
library(openintro)


#### EINFACHE REGRESSION
# Y_i = Gewicht
# X_i = Größe
#   => Durchschnittliches Gewicht m_i = a + X_i
# Annahme: Y_i's sind normalverteilt
# a ~ N(0,200^2), 
# b ~ N(1,0.5^2), 
# s ~ Unif(0,20)

# Beobachten Daten genauer untersuchen
ggplot(bdims, aes(hgt,wgt)) + geom_point() + geom_smooth(method='lm')
wt_mod <- lm(wgt ~ hgt, bdims)
coef(wt_mod)
# (Intercept)         hgt 
# -105.011254    1.017617 
summary(wt_mod)$sigma  # 9.30804


### Priors
# 10000 samples von den priors
a <- rnorm(n = 10000, mean = 0, sd = 200)    
b <- rnorm(n = 10000, mean = 1, sd = 0.5)    
s <- runif(n = 10000, min = 0, max = 20)
samples <- data.frame(set = 1:10000, a, b, s)

ggplot(samples, aes(x = a)) + geom_density()
ggplot(samples, aes(x = b)) + geom_density()
ggplot(samples, aes(x = s)) + geom_density()

## Simulieren von erzeugten Daten zu Prior-Parameter-sets
# Replizieren der der ersten 12 Prior-Parameter-sets - jedes 50 mal
# => 600 Einträge
prior_scenarios_rep <- bind_rows(replicate(n = 50, expr = samples[1:12, ], simplify = FALSE)) 
head(prior_scenarios_rep,24) # => hat set set variable
# Simulieren von 50 Datenpunkten - Größe & Gewicht - für jedes Parameter-set
prior_simulation <- prior_scenarios_rep %>% 
  mutate(height = rnorm(n = 600, mean = 170, sd = 10)) %>% 
  mutate(weight = rnorm(n = 600, mean = a + b * height, sd = s))

ggplot(prior_simulation, aes(x = height, y = weight)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, size = 0.75) + 
  facet_wrap(~ set)
# => range von plausiblen prior models 


## Daten
# = gewichts-datenset von openintro
ggplot(bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


## 1. Definieren des Regressions Modell
weight_model <- "model{
    # Liklihood model für Y[i]
    for(i in 1:length(Y)) {
      Y[i] ~ dnorm(m[i], s^(-2))  # [] => subject-dependent
      m[i] <- a + b * X[i]        # <- weil deterministisch
    }

    # Definieren der a, b, s priors
    a ~ dnorm(0, 200^(-2))
    b ~ dnorm(1, 0.5^(-2))
    s ~ dunif(0, 20)
}"
## 2. Kompilieren des Modells
weight_jags <- jags.model(
  textConnection(weight_model),
  data = list(Y = bdims$wgt, X = bdims$hgt),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989)
)
# 3. Simulieren der posterior
weight_sim <- coda.samples(model = weight_jags, variable.names = c("a", "b", "s"), n.iter = 1000)
plot(weight_sim)
# => nicht stabil - nochmal mit 100.000
weight_sim <- coda.samples(model = weight_jags, variable.names = c("a", "b", "s"), n.iter = 100000)
plot(weight_sim)



#### Punkt-Schätzungen für die Parameter
summary(weight_sim)

# Schätzung von posterior mean von b
weight_chains <- data.frame(weight_sim[[1]], iter = 1:100000)
mean(weight_chains$b)

# Posterior mean regression model
ggplot(bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_abline(intercept = mean(weight_chains$a), slope = mean(weight_chains$b), color = "red")

# Range von 20 posterior regression models
ggplot(bdims, aes(x = hgt, y = wgt)) + 
  geom_point() +
  geom_abline(intercept = weight_chains$a[1:20], slope = weight_chains$b[1:20], color = "gray", size = 0.25)


### Posterior credible intervals
# 95% und 90% posterior credible interval von b
ci_95 <- quantile(weight_chains$b, probs = c(0.025, 0.975))
ci_90 <- quantile(weight_chains$b, probs = c(0.05, 0.95))

ggplot(weight_chains, aes(x = b)) + 
  geom_density() + 
  geom_vline(xintercept = ci_95, color = "red")


### Hypothesen-Tests: Posterior Probabilities 
# Bsp: HA: b <= 1.1
# -> H0: b > 1.1
# Markiere 1.1 bei posterior density plot von b
ggplot(weight_chains, aes(x = b)) + 
  geom_density() + 
  geom_vline(xintercept = 1.1, color = "red")

# Wieviele Chain values sind > 1.1
table(weight_chains$b > 1.1)
# Wie große ist der Anteil
mean(weight_chains$b > 1.1)



### Inferenz für den Posterior trend
# Annäherung des Posterior trend für Gewicht unter 180 großen Menschen
# und die Posterior Unsicherheit bzgl des Trends
# Berechne den trend für jedes Markov chain parameter set
weight_chains <- weight_chains  %>% 
  mutate(m_180 = a + b * 180)

# Posterior density plot für den Trend
ggplot(weight_chains, aes(x = m_180)) + 
  geom_density()

# Credibile Intervals für den Trend
quantile(weight_chains$m_180, c(0.025, 0.975))


### Posterior Vorhersagen
# 100,000 für das Gewicht eines 180 cm Menschen
# Simuliere eine Vorhersage mit dem ersten & zweiten Parameter-set
rnorm(n = 1, mean = weight_chains$m_180[1], sd = weight_chains$s[1])
rnorm(n = 1, mean = weight_chains$m_180[2], sd = weight_chains$s[2])

# Simuliere & speicher eine Vorhersage für jedes Parameter-set
weight_chains <- weight_chains  %>% 
  mutate(Y_180 = rnorm(n = 100000, mean = m_180, sd = s))



### Posterior predictive distribution
# Posterior credible interval für die Vorhersage
ci_180 <- quantile(weight_chains$Y_180, c(0.025, 0.975))

# Credible Interval in Density
ggplot(weight_chains, aes(x = Y_180)) + 
  geom_density() + 
  geom_vline(xintercept = ci_180, color = "red")

# Credible Interval auf Scatterplot
ggplot(bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_abline(intercept = mean(weight_chains$a), slope = mean(weight_chains$b), color = "red") + 
  geom_segment(x = 180, xend = 180, y = ci_180[1], yend = ci_180[2], color = "red")  



