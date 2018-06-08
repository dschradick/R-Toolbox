library(tidyverse)
library(Matching)

# Matching Estimator
# Quasi-experimental method that aims to search for counterfactual unit 
# that is comparable with the treated unit among many untreated units

# Step 1. Estimate logit model to get propensity scores:
# Step 2. Search among the untreated units that have the 
#         smallest differences in the estimated propensity scores with the treated units. 
#         The found units are then used are counterfactual units for imputing 
#         the potential outcomes of the treated units.
# Step 3. Calculate mean difference between observed Y and matched Y. 
#         This mean difference is the Average Treatment effect on the Treated (ATT).

### Simulate Data
set.seed(101)
n <- 1000

# Generate baseline covariates:
W1 <- runif(n, min = 0.02, max = 0.7) 
W2 <- rnorm(n, mean = (0.2 + 0.125*W1), sd = 1) 

# Generate binary treatment indicator
A <- rbinom(n, 1, plogis(-.7 + 1.8*W1 - 1.4*W2))

# Generate the potential outcomes
Y.0 <- rnorm(n, mean = (-.5 + 2*poly(W1,2) - W2), sd=1)
Y.1 <- rnorm(n, mean = (-.5 + 2*poly(W1,2) - W2 + 1.5 + 2*poly(W1,2) - W2), sd=1)

# Generate the observed outcome
Y <- ifelse(A == 1, Y.1, Y.0)
  
data <- data.frame(W1,W2,A,Y,Y.1,Y.0)

# Step 1
logitmodel <- glm(A ~ W1 + W2, data = data, family=binomial) 

## Step 2
data <- mutate(data, pscore = logitmodel$fitted.values) 
data1 <- filter(data, A == 1)                      # Treated units data 
data0 <- filter(data, A == 0)                      # Untreated units data 
match.data <- data1                                # Make a copy of treated units data 
for(i in 1:nrow(match.data)){                      # Now find counterfactual units 
  temp <- data0 %>%                                # Search among untreated units 
    mutate(pscorei = data1$pscore[i],              # PS of the treated unit i 
           dif.score = abs(pscorei - pscore)) %>%  # Score difference
    arrange(dif.score) %>% 
    slice(1) %>%                                   # Choose the top one with lowest score dif
    dplyr::select(!!colnames(match.data))          # Keep needed cols
  match.data[i,] <- temp[1,]                       # Replace with the found unit 
}

# Step 3
mean(data1$Y - match.data$Y) 

## Alternative: Use matching package
m.out <- Match(Y = data$Y, Tr = data$A, X = data$pscore, distance.tolerance = 0)
summary(m.out)
mean(data1$Y.1 - data1$Y.0) # True ATT


