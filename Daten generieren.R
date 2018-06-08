library(tidyverse)
#### Daten Generierung initialisieren
set.seed(0) 

#### Zufallszahlen aus Verteilungsfunktion
runif(n = 100, min = 0, max = 10)        # Uniform
rnorm(n = 100, mean = 100, sd = 15)      # Normal
rpois(n = 100, lambda = 10)              # Poisson
rbinom(n = 100, size = 100, p=.2)        # Binomial
runif(1,min=0,max=1)                     # Zufallszahl 0-1 
 
#### Erzeugungsfunktionen
x <- 1:100
x <- c(1,2,3,4,5)                        
rev(x)                                  # 5 4 3 2 1

sample(x, size = 100, replace=TRUE)

seq(from=0 ,to=1,  by=0.2)              # 0.0 0.2 0.4 0.6 0.8 1.0
seq(from=0 ,to=10, by=2)                # 0  2  4  6  8 10

rep(0,times=10)                         # 0 0 0 0 0 0 0 0 0 0
rep(c(1,2),times=5)                     # 1 2 1 2 1 2 1 2 1 2
rep(c(1,2), times=3, each=2)            # 1 1 2 2 1 1 2 2 1 1 2 2
rep(c('a','b'), times=3, each=2)

seq(as.Date("2017-1-1"), as.Date("2017-1-4"), "days") 
# "2017-01-01" "2017-01-02" "2017-01-03" "2017-01-04"

#### Player Daten
nplayers <- 100
platforms <- c('ios','anroid','amazon')
id <- 1:nplayers

### Einfach
# => Spalten weitestgehend unabhängig
platform <- sample(platforms, size=nplayers, replace=TRUE, prob=c(.3, .4,.3))
logins <- round(rnorm(nplayers, mean = 100, sd=25))
is_payer <- rbinom(nplayers, size = 1, p=.05) 
spent <- logins * is_payer * runif(1,min=0,max=1)     
players <- tibble(id,platform,logins,spent)
View(players)


### Komplexer
# => nacheinander aufeinander aufbauen 
players = tibble(id)
players$logins <- round(rnorm(nplayers, mean = 100, sd=25))
