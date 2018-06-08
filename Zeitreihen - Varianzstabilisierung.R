########## TIME SERIES - VARIANZSTABILISIERUNG
library(tidyverse)
library(forecast)
library(fpp2)

#### Einfacher Log-transform
usmelec %>% autoplot() 
usmelec %>% log() %>% autoplot() 
# => reicht hier nicht

#### Boxcox Transformation
# Zur Stabilisierung der Varianz
# Power-Transform-Stärke: sqrt => cuberoot => log => inverse
#
# if lambda = 0 => w_t  = log(y_t) 
# else           w_t = (y_t^lambda-1) / lambda) 
#
# Lambda: [-1,1]
# 1   = Keine transformation (subtrahiert 1) 
# 1/2 = sqrt
# 1/3 = cube root
# 0   = ln
# -1  = inverse transformation
# => möglicherweise bias-adjustment notwendig (biasadj=TRUE)

autoplot(usmelec)             # => steigende Varianz
autoplot(usmelec^(1/2))       # sqrt:     nur wenig besser
autoplot(usmelec^(1/3))       # cuberoot: besser
autoplot(log(usmelec))        # log:      noch besser - aber immernoch größer hinten
autoplot(-1 / usmelec)        # inverse:  zuviel - hinten zu klein
# ==> irgendwas zwischen log und inverse

usmelec %>% BoxCox(lambda =  0) %>% autoplot()        # 0 = log
usmelec %>% BoxCox(lambda = -1) %>% autoplot()        # -1 = inverse
(lambda <- BoxCox.lambda(usmelec))                    # -0.5738331 
usmelec %>% BoxCox(lambda = -lambda) %>% autoplot()    # Varianzhomogenität erreicht
