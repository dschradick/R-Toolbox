########## MIXTURE DENSITIES
library(tidyverse)

#### Mixture Komponenten
mixture_component_1 <- function(){ rnorm(1,100,10) }
mixture_component_2 <- function(){ rnorm(1,150,10) }

### !!!WAS ES NICHT IST!!! - Addieren der Werte 
#  => erzeugt wieder Normalverteilung!!!!
get_sum_sample <- function(){
  return(mixture_component_1() + mixture_component_2())
}

#### Formulierung 
# 1. Auswahl nach Zufall mit Gewichtung
# 2. Sampling
get_mixture_sample<- function(){
  if (runif(1) > 0.5)
    mixture_component_1()
  else 
    mixture_component_2()
}


mc_1 <- c(); mc_2 <- c(); sums <- c() ;  mix_1 <- c(); mix_2 <- c()
for (i in 1:10000) {
  mc_1 <- append(mc_1, mixture_component_1())
  mc_2 <- append(mc_2, mixture_component_2())
  sums <- append(sums, get_sum_sample())
  mix_1 <- append(mix_1, get_mixture_sample())
}

## Komponenten
plot(density(mc_1))
plot(density(mc_2))
## Mixture
plot(density(mix_1))


## Alle zusammen plotten
d_mix_1 <- density(mix_1)
d_mc_1 <- density(mc_1)
d_mc_2 <- density(mc_2)

x <- d_mix_1$x %>% append(d_mc_1$x) %>% append(d_mc_2$x)
distribution <- rep('mixture_density',512) %>%
  append(rep('m_component_1',512)) %>%
  append(rep('m_component_2',512)) 
y <- d_mix_1$y %>% 
  append(d_mc_1$y*1/2) %>%
  append(d_mc_2$y*1/2) 

df <- tibble(x,distribution,y)

ggplot(df,aes(x,y,color=distribution)) + geom_line()



