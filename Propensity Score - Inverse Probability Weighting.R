library(tidyverse)
library(Matching) 

data(lalonde)
options(scipen=999)
x <- mtcars
# IPW
# Quasi-experimental method for estimating causal effects 
# under the assumption of conditional independence. 

## Problem OLS:
# OLS places more weights on those covariates 
# that are dissimilar in their attributes. 
# This can be problematic because the identification 
# would thus have to rely on extrapolation (inducing extrapolation bias).

## Solution: IPW
# IPW helps avoid extrapolation. 
# Unlike OLS, IPW places more weights on observations 
# that are similar to each other in the covariates, 
# improving on the covariate balance. This is the reverse of what the OLS does

###### IPW in two steps
# 1. Logit model to estimate the probability of being treated.
# 2. Weighted Least Squares to estimate the effect of W on Y
#    weight = inverse of estimated probability
#    => 1/p for treated & 1/(1-p) for untreated
ols <- lm(re78 ~ treat + age + educ + black + hisp + married + nodegr + re74 + re75, data = lalonde)
summary(ols) 
# treat = 1676.34322
lalonde
ps <- glm(treat ~ age + educ + black + hisp + married + nodegr + re74 + re75, data = lalonde, family = binomial(link = 'logit'))
lalonde <- lalonde %>%
  mutate(prob = predict(ps, type = 'response')) %>%
  mutate(w = treat/prob + (1-treat)/(1-prob)) 

lalonde %>%
  group_by(treat) %>%
  select_if(is.numeric) %>%
  summarise_all(mean)

lalonde$treat_c <- factor(lalonde$treat) 
ggplot(lalonde, aes(re78, fill=treat_c)) + geom_histogram(position = 'identity',  alpha=0.6)
ggplot(lalonde, aes(re78, fill=treat_c, weight=w)) + geom_histogram(position = 'identity',  alpha=0.6)

# Weighted least squares estimation 
ipwreg <- lm(re78 ~ treat + age + educ + black + hisp + married + nodegr + re74 + re75, data = lalonde, weights = w)
summary(ipwreg) # Output omitted
# treat = 1637.15390
# Slightly lower than the OLS estimate. 
# The minor difference between OLS and IPW estimates is due 
# to that the program participation was randomly assigned. 
# So the participants and non-participants are in fact not 
# very different to each other. 
# If they are, we would likely see a larger difference in the results.
