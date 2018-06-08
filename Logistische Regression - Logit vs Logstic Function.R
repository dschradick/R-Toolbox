library(tidyverse)

# Model
mod <- lm(vs ~ hp , data=mtcars)

# Predict log-odds
z <- predict(mod, 
             newdata = data.frame(hp=c(150)), 
             type='response')

# Logit:    Proba    => Log-Odds
# Logistic: Log-Odds => Proba
logit    <- function(p){log(p / (1-p))}    # Link unction
logistic <- function(z){1 / (1+exp(-z))}   # Inverse Link Function

# Predictions
z                            # 0.4198921
(proba = logistic(z))        # 0.6034574

# Check both ways
(logodds = logit(proba))     # 0.4198921
(proba = logistic(logodds))  # 0.6034574    √


df <- tibble(hp = seq(-600,600))
df$z     <- predict(mod,data.frame(hp=df$hp))
df$prob  <- logistic(df$z)
df <- pivot_longer(df, names_to='scale', cols=c('z','prob'))
ggplot(df, aes(x=hp, y=value, color=scale)) +
  geom_line() 


### Risk-Ratio vs Odds-Ratio
# 60% vs 30% 
# RR:  0.6 / 0.3             # 2.0
#  =  (6 / 10) / (3 / 10)     
# OR: (6 /  4) / (3 /  7)    # 3.5

