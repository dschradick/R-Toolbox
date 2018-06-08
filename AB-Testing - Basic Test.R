########## MARKETING - AB-TEST
library(tidyverse)
library(broom)

#### Daten einlesen
file <- "~/Documents/Data/ABtestClickThrough.csv"
data <- read_csv(file) 

#### Daten vorbereiten
str(data)
names(data) <- c("group", "time", "clicked")
data$group <- as_factor(data$group)
data$clickedTrue <- as_factor(data$clicked) 
levels(data$clickedTrue) <- c("0", "1")
glimpse(data)


#### Häufigkeitstabelle
(freqTable <- table(data$group, data$clicked))
##       1   0
##   A  20 480
##   B  40 460


#### Signifikanztest
chisq.test(freqTable, simulate.p.value = F) %>% tidy()

# two-proportions z-test 
prop.test(freqTable, conf.level = .95)
prop.test(freqTable, conf.level = .95, correct = F ) %>% tidy()
prop.test(freqTable, conf.level = .95) %>% tidy()
# => gleicher p-value = 0.01141
## sample estimates:
## prop 1 prop 2 
##   0.04   0.08


### chi-squared vs binomial approx

## Binomial Approximation
test_data <- data %>%
    group_by(group) %>%
    summarize(n = n(),
              p = mean(clicked),
              mu = n*p,
              var = n*p*(1-p))

test_data %>% pivot_wider(id_cols = group, )
              
#            x = z.test(x, sigma.x=0.5, y, sigma.y=0.5, mu=2))
    

p <- data %>% summarise(mean = mean(clicked)) %>% pull()
n_a <- data %>% filter(group == 'A') %>% count() %>% pull()
n_b <- data %>% filter(group == 'B') %>% count() %>% pull()
n <- n_a + n_b
p_a <- data %>% filter(group == 'A') %>% summarise(mean = mean(clicked)) %>% pull()
p_b <- data %>% filter(group == 'B') %>% summarise(mean = mean(clicked)) %>% pull()

mu_a  <- n_a * p_a
mu_b  <- n_b * p_b
var_a <- n_a * p_a * (1 - p_a)
var_b <- n_b * p_b * (1 - p_b)

hist(rnorm(mu_a,sd = sqrt(var_a),n = 10000), breaks=30)
hist(rnorm(mu_b,sd = sqrt(var_b),n = 10000), breaks=30)


# https://www.statisticshowto.com/z-test/
# https://de.wikipedia.org/wiki/Gauß-Test#Zweistichproben-Gauß-Test_für_unabhängige_Stichproben
z = ((p_a - p_b) ) /
     (sqrt(p * (1 - p) * (1/n_a + 1/n_b)))

pnorm(z) * 2 # richtig

#https://online.stat.psu.edu/stat200/book/export/html/193

# ---



# Comparing Two Population Proportions
#https://online.stat.psu.edu/stat500/lesson/7/7.2/7.2.2
z = ((p_a - p_b) /
      sqrt(p * (1-p) * (1/n_a + 1/n_b)))
z
pnorm(z) * 2



#---
# Theorem: Difference of two independent normal variables
# https://online.stat.psu.edu/stat500/lesson/7/7.1

# ABER WARUM : "follows normal dist with mean mu_a - mu_b"
# und dann bekommt man hier nicht 0?!

mu_ <- mu_a - mu_b
sd_ <- sqrt(((var_a) + (var_b))) # => bekannt - kein sd notwendig
z = mu_ / sd_
z
pnorm(z) * 2


# ----



# Inference for Independent Means
# => uses stderr => then t-test
# https://online.stat.psu.edu/stat500/lesson/7/7.3/7.3.1
mu_ <- mu_a - mu_b
stderr_ <- sqrt( (var_a/n_a) + (var_b/n_b))                   # problem - unequal variance => unkorrekte ergebnis
stderr_ <- sqrt(((n_a - 1) * var_a + (n_b - 1) * var_b)       # braucht pooled variance 
                /(n_a + n_b - 2))
t = mu_ / stderr_
t
pt(t,n_a + n_b - 2) * 2    

mu_a = n_a *  p_a
mu_b = n_b *  p_b
