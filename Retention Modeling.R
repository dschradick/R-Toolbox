########## RETENTION MODELING
library(RPostgreSQL)
library(dplyr)
library(gmodels)
library(data.table)
library(ggplot2)
library(lubridate)
library(MASS)
library(fitdistrplus)
library(actuar)
library(stats4)
library(tidyr)


setwd("~/R/GameAnalysis/")
source('DBConnector.R')

#### Daten lesen
query.file <- 'sql/PlaytimeRetention.sql'
retention.query <- readChar(query.file, file.info(query.file)$size)
dbh <- connect.to.gamedb()
query <- retention.query
retention <- dbGetQuery(dbh, query )
print(object.size(retention),units='Mb')
par(mfrow=c(1,1))

dim(retention)
head(retention)

#### Daten vorbereiten
#retention <- subset(retention, playtime > 60)
retention$playtime <- round(retention$playtime / 60) 
summary(retention)
ggplot(retention,aes(playtime)) + stat_density()



#### Fitting
data <- retention$playtime
#summary(data)
est.weibull <- mle(weibull_loglnk, start = list(shape=2, scale=1))
est.gamma <- mle(gamma_loglnk, start = list(shape=2, rate=1))
est.pareto <- mle(pareto_loglnk, start = list(shape=2, scale=1))
est.exponential <- mle(exp_loglnk, start = list(rate=.5))
#est.exponential <- fitdistr(data, "exponential")$estimate



## Using samples
fitted.weibull <- rweibull(n=10000, shape=coef(est.weibull)[1], scale=coef(est.weibull)[2])
fitted.gamma <- rgamma(n=10000, shape=coef(est.gamma)[1], rate=coef(est.gamma)[2])
fitted.exponential <- rexp(n=10000, rate=est.exponential['rate'])
fitted.pareto <- rpareto(n=10000, shape=coef(est.pareto)[1], scale=coef(est.pareto)[2])

w <- (fitted.weibull-mean(fitted.weibull))/sd(fitted.weibull)
p <- (fitted.pareto-mean(fitted.pareto))/sd(fitted.pareto)
qqplot(sample(data,10000,replace = T),fitted.pareto)
abline(0,1)

to.plot <- data.frame(raw=sample(data,10000,replace = T),
                      fitted.weibull=fitted.weibull,
                      fitted.gamma=fitted.gamma,
                      fitted.exponential=fitted.exponential,
                      fitted.pareto=fitted.pareto)


to.plot <- round(to.plot)
to.plot <- gather(to.plot)
names(to.plot) <- c('distribution','value')
ggplot(to.plot,aes(value,color=distribution)) + 
  geom_density() +
  xlim(0,500) 



## Playtime Density erzeugen
counts <- retention %>% count(playtime)
counts$density <- counts$n / sum(counts$n)
dplaytime <- function(x){
  counts$density[x] 
}

dplaytime(100000)

### Direkt mit dFunktionen
x_lower <- 5 
x_upper <- 500
ggplot(data.frame(x = c(x_lower, x_upper)), aes(x = x)) + xlim(x_lower, x_upper) + 
  stat_function(fun = dweibull, args = list(shape=coef(est.weibull)[1], scale=coef(est.weibull)[2]), aes(colour = "0.5")) + 
  stat_function(fun = dpareto, args = list(shape=coef(est.pareto)[1], scale=coef(est.pareto)[2]), aes(colour = "1")) + 
  stat_function(fun = dexp, args = list(rate=coef(est.exponential)[1]), aes(colour = "2")) + 
  stat_function(fun = dgamma, args = list(shape=coef(est.gamma)[1], rate=coef(est.gamma)[2]), aes(colour = "3")) + 
  stat_function(fun = dplaytime, aes(colour = "4")) + 
  scale_color_manual("Rate", values = c("yellow", "green", "red", "purple",'blue')) +
  labs(x = "\n x", y = "f(x) \n", title = "Distribution Density Plots \n") + 
  theme(plot.title = element_text(hjust = 0.5), 
    axis.title.x = element_text(face="bold", colour="blue", size = 12),
    axis.title.y = element_text(face="bold", colour="blue", size = 12),
    legend.title = element_text(face="bold", size = 10),
    legend.position = "right")


plot(density(data))
lines(density(fitted.gamma))

##### Loglikihood functions
exp_loglnk <- function(rate){
  n <- length(data)
  loglik <- sum(dexp(data, rate=rate, log=TRUE))
  return(-loglik)
}

gamma_loglnk <- function(shape, rate){
  n <- length(data)
  loglik <- sum(dgamma(data, shape=shape, rate=rate, log=TRUE))
  return(-loglik)
}

weibull_loglnk <- function(shape, scale){
  n <- length(data)
  loglik <- sum(dweibull(data, shape=shape, scale=scale, log=TRUE))
  return(-loglik)
}

pareto_loglnk <- function(shape, scale){
  n <- length(data)
  loglik <- sum(dpareto(data, shape=shape, scale=scale, log=TRUE))
  return(-loglik)
}


