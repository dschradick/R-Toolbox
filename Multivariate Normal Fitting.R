library(mvtnorm)

#### Daten simulieren
set.seed(0)
df <- data.frame(
  x = rnorm(1000, mean=80, sd=20),
  y = rnorm(1000, mean=0, sd=5),
  z = rnorm(1000, mean=0, sd=5)
)

#### Daten betrachten
head(df)
summary(df)

#### Parameter schätzen
df_means <- c(x=mean(df$x), 
              y=mean(df$y), 
              z=mean(df$z))
df_cov <- cov(df)             # Benutzt Kovarianz-Matrix nicht Korrelations-Matrix!!!


# P(x <= 80)
sum(df$x <= 80) / nrow(df)  
pmvnorm(lower=rep(-Inf, 3), upper=c(80, Inf, Inf), mean=df_means, sigma=df_cov)
# Ohne Simulation
pmvnorm(lower=rep(-Inf, 3), upper=c(80, Inf, Inf), mean=c(80,0,0), corr=diag(rep(1,3)))

