########## LOGISTISCHE REGRESSION - BASICS
library(dplyr)
library(ggplot2)
library(broom)
library(plotly)
library(modelr)
library(RColorBrewer)
library(plotly)
#install.packages('Stat2Data')
library(Stat2Data)
library(openintro)
data("MedGPA")

qplot(marioKart$sellerRate, marioKart$totalPr, geom='point')

# Linear probability model
# Problem: prediction < 0 und > 1
ggplot(MedGPA,aes(x=GPA,y=Acceptance)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.5) +
  geom_smooth(method="lm",se=FALSE)


## Noch möglich: Observations nahe Mittelwert der unabhängigen Variablen
# => dann normale lineare Regression noch möglich, selbst für binäre Response
MedGPA_middle <- filter(MedGPA, GPA >= 3.375 & GPA <= 3.77)
ggplot(MedGPA_middle,aes(GPA,Acceptance)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.5) + 
  geom_smooth(method = 'lm', se=F)


#### Visualisierung
# Für Werte in der Mitte auch mit lm relativ gute Vorhersage
# => eigentlich alle Punkte auf selben Wert =>  jitte
# => GPA = 3.5 = 50% of acceptance
ggplot(MedGPA,aes(x=GPA,y=Acceptance)) + 
  geom_jitter(width = 0, height = 0.01, alpha = 0.5) +
  geom_smooth(method="lm",se=F, color='blue') +
  geom_smooth(method="glm",se=T, color='red',
              method.args = list(family="binomial")) +
  xlab('GPA') + ylab('Acceptance')


#### Generalized linear model 
# Erlaubt, dass abhängige Variable eine andere Verteilung als Normalverteilung besitzt 
# Ermöglicht durch Link-Funktion, welche lineares Modell auf response abbildet
# => keine geschlossene Lösung => MLE
model <- glm(Acceptance ~ GPA, data = MedGPA, family = 'binomial')
summary(model)
coef(model)
confint(model)


#### Binning
# Hier zur Visualisierung wie binäre response sich ändert
# In jedem bin die entsprechende gesuchte Proportion 
# = Proportion von akzeptierten Studenten
#
#MedGPA$bin = cut(MedGPA$GPA,breaks = seq(min(MedGPA$GPA),max(MedGPA$GPA),by=0.25)) 
MedGPA$bin = cut(MedGPA$GPA,breaks = c(2.72,3.3,3.44,3.58,3.7,3.87,3.97)) 
MedGPA_binned <- MedGPA %>%
  group_by(bin) %>%
  summarize(mean_GPA = mean(GPA), acceptance_rate = mean(Acceptance)) %>%
  filter(acceptance_rate > 0)
MedGPA_binned[1,3] = 0.200

mod <- glm(formula = Acceptance ~ GPA, family = binomial, data = MedGPA)
MedGPA_plus <- mod %>%
  augment(type.predict = "response")

ggplot(data = MedGPA_binned, aes(x = mean_GPA, y = acceptance_rate)) + 
  geom_point() + 
  geom_line() + 
  geom_line(data = MedGPA_plus, aes(x = GPA, y = .fitted), color = "red")


#### Augment
head(augment(mod),2)[1:7] # versus
head(augment(mod,type.predict = "response"),2)[1:7]


#### Out-Of-Sample prediction (probabilty)
new_data <- data.frame(GPA=3.51)
augment(mod, newdata=new_data, type.predict="response")
# Out-Of-Sample prediction (binary) + Confusion
tidy_mod <- augment(mod, type.predict = "response") %>% 
  mutate(Acceptance_hat = round(.fitted))
tidy_mod %>%
  select(Acceptance, Acceptance_hat) %>% 
  table()

