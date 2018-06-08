########## SAMPLE GRÖßE UND EFFEKTSTÄRKE
library(pwr)

#### Test-Daten erzeugen
set.seed(0)
x <- rnorm(n = 100 ,mean = 100, sd = 15)
y <- rnorm(n = 100 ,mean = 105, sd = 15)

#### Einseitiger one-sample t-test
t.test(x, mu = 99, alternative = 'greater')
# => hier möglich sein H0 abzulehnen bei alpha = 0.05

#### Test mit vorgegebenener Effekt-Größe
effect.size = 2
y.minus.effect <- y - effect.size
t.test(x, y.minus.effect, alternative = 'less')

#### Sample Size berechnen
power <- .90                  # Power = Wahrscheinlichkeit von Type II Fehler
sig.level <- .05              # Signifikanz = Wahrscheinlichkeit von Type I Fehler
effect.size <- 5              # gewünschter Unterschied
sigma <- 15                   # (empirische) Standard-Abweichung
cohensd <- effect.size/sigma  # Cohen's D berechnen
(pwrt <- pwr.t.test(d=cohensd, sig.level = sig.level, power = power, type = 'two.sample'))
t.test(x, y)


#### Erreichte Power bei gegebener Sample-größe berechnen
# n = größer jeder gruppe
n = 100  
pwr.t.test(n = n, d=cohensd, sig.level = sig.level, type = 'two.sample')


#### (Derzeit) belegbare Effektstärke berechnen
# n = größer jeder gruppe
# Je größer Sample, desto kleiner kann zeigbare Effektstärke sein
# => z.B. 0.5% increase braucht viel mehr samples als 10% increase
result <- pwr.t.test(n = 150, sig.level = sig.level, power = power, type = 'two.sample')
cat("Dezeit minimal festgestellbarer Effekt:", result$d * sigma)


### Power Funktion plotten
pwrt <- pwr.t.test(d=.7,n=c(10,20,30,40,50,60,70,80,90,100),
                   sig.level=.05,type="two.sample",alternative="two.sided")
plot(pwrt$n,pwrt$power,type="b",xlab="sample size",ylab="power")

