########## BOOTSTRAPPING MIT BOOT PACKAGE
library(boot) 

## Normales Bootstrapping
# zweites Argument muss index für boot funktion sein
boot.mean <- function(data,index){
  sub <- data[index]
  return(mean(sub))
}

boot.obj <-boot(0:10,boot.mean,1000) # => Mean: 5, std.error = 0.95
boot.obj

hist(boot.obj$t)
quantile(boot.obj$t,c(0.025,0.5,.975))

r <-boot.ci(boot.obj, conf=.95, type="bca")          # Confidence Interval
r$bca


## Boostrapping von linearem regressions-modell
boot.fn <- function(data,index)  
  return(coef(lm(mpg ~ horsepower, data=data, subset=index)))

boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000) # data, statistic, NumberOfReplicates

summary(lm(mpg~horsepower,data=Auto))$coef # vgl


