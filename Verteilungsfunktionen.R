########## VERTEILUNGS-FUNKTIONEN
library(tidyverse)
library(modelr)
library(broom)

## RNORM	
# Generates random numbers from normal distribution	
# rnorm(n, mean, sd)	
# Generates 1000 numbers from a normal with mean 3 and sd=.25
rnorm(1000, 3, .25)

hist_info <- hist(rnorm(1000, 3, .25),prob=T)
hist_info

## DNORM
# Probability Density Function
# dnorm(x, mean, sd)	
# Gives the density (height of the PDF) of the normal with mean=0 and sd=.5. 
dnorm(0, 0, .5)
x <- seq(from= -2, to=2, by = 0.01)
plot(x,dnorm(x, 0, .5), type = 'l')


## PNORM	
# Cumulative Distribution Function (CDF)	
# pnorm(q, mean, sd)	
# Gives the area under the standard normal curve to the left of 1.96, i.e. ~0.975
pnorm(1.96, 0, 1, lower.tail=TRUE)
pnorm(1.96, 0, 1, lower.tail=FALSE)
plot(x,pnorm(x, 0, .5), type = 'l')

## QNORM	
# Quantile Function – inverse of pnorm	
# qnorm(p, mean, sd)	
# Gives the value at which the CDF of the standard normal is .975, i.e. ~1.96
qnorm(0.975, 0, 1) 



## Beispiel: Binomial
dbinom(180,  prob=.9, size=200)        # Denstiy
pbinom(q=90,  prob=.9, size=200)       # CDF           (distribution function)
qbinom(p=.55, prob=.9, size=100)       # inverse CDF   (quantile function)
# Plotten
x <- seq(from=0, to=120, by=1)
plot(x,dbinom(x, size=100, p=.9),type='l')
plot(x,pbinom(x, size=100, p=.9),type='l')

