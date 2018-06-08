### Übertragen von Stata-Effects Docs
library(tidyverse)
library(modelr)
library(broom)
library(haven)
library(scales)


c1 <- hue_pal()(2)[1]; c2 <- hue_pal()(2)[2]
data <- read_dta("~/Documents/Data/bweightex.dta")
#data <- read_dta("https://www.stata-press.com/data/r17/cattaneo2.dta")
data2 <- read_dta("https://www.stata-press.com/data/r17/cattaneo2.dta")
# => falls ANDERE Ergebnisse => weil erster Datensatz nur Auszug 
# mit zweiten datensatz bzgl Regression adjustment getestet

data <- data %>% mutate(mbsmoke = factor(data$mbsmoke))  #%>% select(bweight, mbsmoke, mage)
data2 <- data2 %>% mutate(mbsmoke = factor(data2$mbsmoke))

## ZIEL: Effekt von smoking auf birth weight
# Problem: Vermischung des negativen Effekts des Rauchens mit postivem Effekt des Alters
# => notwendig - overlap bzgl Alter in beiden Gruppen, damit diese verglichen werden können

ggplot(data, aes(mage, bweight, color=mbsmoke)) + 
  geom_point() + 
  geom_smooth(method='lm', se=F)

lm(bweight ~ mage + mbsmoke, data=data) %>% tidy()
lm(bweight ~ mage * mbsmoke, data=data) %>% tidy()

## Überprüfen des Overlap
ggplot(data, aes(mage, bweight, color=mbsmoke)) + geom_point() 
# => Ja, overlap bzgl Alter vorhanden 
# => könnte als 
treatment_model <- glm(mbsmoke ~ prenatal1 + mmarried + I(mage*mage) + fbaby, family=binomial, data)
treatment_model <- glm(mbsmoke ~ mage, family=binomial, data)
data$ps <- predict(treatment_model, data, type='response')
ggplot(data, aes(ps, fill=mbsmoke, color=mbsmoke)) + geom_density(alpha=0.3)
ggplot(data, aes(ps, fill=mbsmoke, fill=mbsmoke)) + geom_histogram(alpha=0.3, position = 'identity')




## Naive - 
naive_mod <- lm(bweight ~ mage + mbsmoke, data)
data %>%
  add_predictions(naive_mod) %>%
  ggplot(aes(mage, pred, color=mbsmoke)) + 
  geom_point(aes(mage, bweight)) + 
  geom_line(size=1)
naive_mod


## Regression adjustment
ggplot(data, aes(mage,bweight, color=mbsmoke)) + 
  geom_point() + 
  geom_smooth(method='lm', se=F) + 
  geom_segment(x = 25, y = 3230, xend = 25, yend = 3620,color='black', )

# => Effect for 25 year old male is vertical distance between lines

#mod_smoke_0 <- lm(bweight ~ prenatal1 + mmarried + mage + fbaby, data %>% filter(mbsmoke==0))
#mod_smoke_1 <- lm(bweight ~ prenatal1 + mmarried + mage + fbaby, data %>% filter(mbsmoke==1))

mod_smoke_0 <- lm(bweight ~ mage, data %>% filter(mbsmoke==0))
mod_smoke_1 <- lm(bweight ~ mage, data %>% filter(mbsmoke==1))

data <- data %>%
  mutate(bw_0 = predict(mod_smoke_0, data),
         bw_1 = predict(mod_smoke_1, data),
         bw_diff = bw_1 - bw_0) 

ggplot(data, aes(mage, bweight, color=mbsmoke)) +
  geom_point() + 
  scale_color_manual(values=c(c1,c2,c2,c1)) +
  geom_point(data=data%>%filter(mbsmoke==0), 
             aes(mage, bw_1, color=c1), shape=1) + 
  geom_point(data=data%>%filter(mbsmoke==1), 
             aes(mage, bw_0, color=c2), shape=1)


(pom_0 <- mean(data$bw_0)) # counterfactual 
(pom_1 <- mean(data$bw_1))
(ate <- mean(data$bw_diff))  # √
(att <- mean((data %>% filter(mbsmoke==1))$bw_diff))
# => erlaubt subject level 


## IPW
# Modellieren des Selection process, für self-selection in smoking
treatment_model <- glm(mbsmoke ~ mage, family=binomial, data=data)

data <- data %>% 
  mutate(ps = predict(treatment_model, data, type='response'),
         ipw = if_else(mbsmoke == 1, 1/ps, 1/(1-ps)))
# Je atyptischer => je weiter propensity von wahren label entfernt => desto höher ipw
# eg. bekommt treatment, obwohl sehr unwahrscheinlich treatment bekommt => hoher wert
# => spiegelt wieder wie gut es als match für die andere klasse fungiert
#    Ein nicht behandelter, der propensity = 1% hat, wird schlecht ein match finden => niedriges gewicht
#
# IST METHODE ZUR ERZEUGUNG VON SYNTHETISCHEN SAMPLES 
# & ARBEITET IM WESENTLICHEN AUF SAMPLING EBENE
# lm modelliert E(Y|X) => aber für eine kombinationen in x keine / wenige Daten um die Schätzung zu machen
# # ipw erzeugt kopien von datenpunkten in unwahrscheinlichen bereichen
# ipw = 10 => lm(weights=ipw) => für 9 weiteren KOPIEN des Datenpunktes
# je unwahrscheinlicher der wert, desto mehr kopien angelegt

## Weighted Average Methode
ggplot(data, aes(mage, fill=mbsmoke)) + geom_boxplot(alpha=0.6)
ggplot(data, aes(mage, fill=mbsmoke, weight=ipw)) + geom_boxplot(alpha=0.6)
# => besser aber noch keine covariate balance 

# Weighted mean als Schätzer (vs Lineare Regression)
d <- data %>%
  group_by(mbsmoke) %>%
  summarize(bweight_naive = mean(bweight),
            bweight_ipw = weighted.mean(bweight, w=ipw))

d$bweight_naive[1] - d$bweight_naive[2]
d$bweight_ipw[1] - d$bweight_ipw[2] 
# => warum anderes Ergebnis als bei Linear?!
#    DASSELBE Ergebnis als wenn Intercept model mit weights
#    mod_smoke_0 <- lm(bweight ~ 1, weights=ipw, data=data %>% filter(mbsmoke==0))
#    ~ avg vs regression 


## Linear Regression Methode
# 1. POM+ate+att model
mod_smoke_0 <- lm(bweight ~ mage, weights=ipw, data=data %>% filter(mbsmoke==0))
mod_smoke_1 <- lm(bweight ~ mage, weights=ipw, data=data %>% filter(mbsmoke==1))

data <- data %>%
  mutate(bw_0_weighted = predict(mod_smoke_0, data), 
         bw_1_weighted = predict(mod_smoke_1, data),
         bw_diff_weighted = bw_1_weighted - bw_0_weighted)

(pom_0 <- mean(data$bw_0_weighted)) # counterfactual 
(pom_1 <- mean(data$bw_1_weighted))
(ate <- mean(data$bw_diff_weighted))
(att <- mean((data %>% filter(mbsmoke==1))$bw_diff_weighted))
# was ist att mit regressions formulierung?


# 2. Regressions modell 
# ate
ipwreg <- lm(bweight ~ mbsmoke + mage + ps, data=data, weights = data$ipw)
mean(predict(ipwreg, newdata=(data %>% mutate(mbsmoke="1")))) - mean(predict(ipwreg, newdata=data %>% mutate(mbsmoke="0")))
ipwreg # coef gleich vorige zeile - interesting... :D
# aber in 1. vorher abzogen dann gemittelt

#ggplot(data=data, aes(ps, color=mbsmoke)) +  geom_density()

ggplot(data, aes(mage, bweight, color=mbsmoke, size=ipw)) + 
  geom_point() 

ggplot(data, aes(mage, color=mbsmoke)) + 
   geom_point(aes(y=bw_0_weighted, size=ipw, color=c1)) +
   geom_point(aes(y=bw_1_weighted, size=ipw, color=c2)) 

ggplot(data, aes(mage, color=mbsmoke)) + 
  geom_point(aes(y=bw_0,color=c1), size=0.5) +
  geom_point(aes(y=bw_1,color=c2), size=0.5) +
  geom_point(aes(y=bw_0_weighted), size=0.5) +
  geom_point(aes(y=bw_1_weighted), size=0.5) 

#ms0 <- lm(bweight ~ mage, data=data %>% filter(mbsmoke==0))
#weighted.mean(predict(ms0, data), data$ipw)


#View(data2)
ggplot(data2, aes(medu, color=mbsmoke)) + geom_density()


## CBPS
library(CBPS)
options(scipen = 999)
fit <- CBPS(mbsmoke ~ mage, method='exact', data)
summary(fit)
data$cbps <- as.numeric(fitted(fit))
data <- data %>% 
  mutate(ipw_cbps = if_else(mbsmoke == 1, 1/cbps, 1/(1-cbps)))

ggplot(data, aes(cbps, fill=mbsmoke, color=mbsmoke)) + geom_density(alpha=0.3)
ggplot(data, aes(mage, fill=mbsmoke)) + geom_boxplot(alpha=0.6)
ggplot(data, aes(mage, fill=mbsmoke, weight=ipw)) + geom_boxplot(alpha=0.6)
ggplot(data, aes(mage, fill=mbsmoke, weight=ipw_cbps)) + geom_boxplot(alpha=0.6)

       