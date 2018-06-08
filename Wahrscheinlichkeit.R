########## WAHRSCHEINLICHKEIT
# Statistische Inferenz = untersucht wie man zu beobachteten Daten ein unterliegendes Modell zu erstellt
# Wahrscheinlichkeit = untersucht wie man Daten von einem Modell erzeugt
# => zwei Richtungen
library(ggplot2)

### Binomial-Verteilung
## Simulation
# n = Versuche
# size = "Münzwürfe" pro Versuch
rbinom(n=1, size=1, p=.5)    # 1 mal  eine Münze werfen
rbinom(n=10, size=1, p=.5)   # 10 mal eine Münze werfen
rbinom(n=1, size=10, p=.5)   # 1 mal  10 Münzen werfen   
rbinom(n=10, size=10, p=.5)  # 10 mal 10 Münzen werfen
rbinom(n=10, size=10, p=.7)  # Unfaire Münze
# => Ergebnis binomial verteilt X_{1..n} ~ Binomial(size,p) 

### Wahrscheinlichkeitsdichtefunktion
# Ist p = Erfolgswahrscheinlichkeit bei einem Versuch, n = Anzahl der Versuche
# => dann ist B(n,p,k) die Wahrscheinlichkeit genau k Erfolge zu erzielen 
# X ~ Binomial(10,.5)
# Pr(X = 5) 
# = Wahrscheinlichkeit, dass man unter der Annahme, 
# dass X wie gegeben verteilt ist, als Ergebnis 5 bekommt
# = Wahrscheinlich genau 5 Heads in 10 Würfen zu bekommen

## Schätzen durch Simulation
flips <- rbinom(n=100000, size=10, p=.5)
mean(flips==5) # 0.25 = 25% Wahrscheinlichkeit 
# => Wahrscheinlichkeitsdichte am Punkt x=5

## Berechnung der exakten Wahrscheinlichkeitsdichte am Punkt x
dbinom(x=5, size=10, p=0.5)
       

### Wahrscheinlichkeitsverteilung
## X ~ Binomial(10,.5)
## Pr(X <= 4) 
## = Wahrscheinlichkeit, dass weniger als 5 Münzen head

## Schätzen durch Simulation
flips <- rbinom(n=100000, size=10, p=.5)
mean(flips==1) + mean(flips==2) + mean(flips==3) + mean(flips==4)
mean(flips<=4)

## Berechnung der Verteilungsfunktion am Punkt x
pbinom(q=4,size=10,p=.5)

## Pr(X >= 5)
mean(rbinom(n=10000, size=10, p=.5) >= 5)
1 - pbinom(q=4, size=10, p=.5)


#### Erwartungswert
# = Mittelwert der Verteilung 
# = Mittelwert, wenn man unendlich viele Versuche durchführt
# X ~ Binomial(size,p) 
# => E[X] = size * p
size = 10
p = .5
## Simulation
flips <- rbinom(n=100000, size=size, p=p)
mean(flips)
## Berechnen
(E_X <- size * p)

#### Varianz
# = mittlere quadratische Abweichung einer Zufallsvariablen von ihrem Erwartungswert
# Oder mittlere quadratische Entfernung von jedem Wert vom Mittelwert des Samples
# X ~ Binomial(size,p) 
# => Var(X) = size * p * (1 - p)
size = 10
p = .5
## Simulation
X <- rbinom(n=100000, size=size, p=p)
var(X) # ~2.5
# => 2.5 ist die mittlere quadratische Abweichung zwischen5 und dem Ergebnis eines Zufallsversuch
## Berchnen
(size * p * (1 - p))


##### Gesetze für Ereignisse
# Ereignis: ist Teil einer Menge von Ergebnissen eines Zufallsexperiments, 
#           dem eine Wahrscheinlichkeit zugeordnet werden kann
# Bsp: Ereignis „eine gerade Zahl zu würfeln“: {2, 4, 6} 
#               ist Teilmenge  der Gesamtmenge {1, 2, 3, 4, 5, 6} 
#               => zugeordnete Wahrscheinlichkeit: 50%
# Bsp: Ereignis: "Kopf bei Münzwurf" => 50% Wahrscheinlichkeit zugeornet
#


## Wahrscheinlichkeit von (Ereignis) A UND (Ereignis) B
# Pr(A und B) = Pr(A) * Pr(B) 
# => wenn, A und B unabhängig 
# => Baumdiagram
# Zwei Münzen Kopf 
A <- rbinom(n=100000, size=1, p=.5)
B <- rbinom(n=100000, size=1, p=.5)
mean(A & B)
(0.5 * 0.5)

## Wahrscheinlichkeit von A ODER B
# Pr(A oder B) = Pr(A) + Pr(B) − Pr(A und B)
# => analog für drei events
A <- rbinom(n=100000, size=1, p=.5)
B <- rbinom(n=100000, size=1, p=.5)
mean(A | B)
(0.5 + 0.5 - (0.5 * 0.5))

# X,Y jeweils 10 Münzwürfe
# Wahrscheinlichkeit, dass X und Y beide kleiner 5
X <- rbinom(100000, 10, .5)
Y <- rbinom(100000, 10, .5)
mean(X <= 4 | Y <= 4)
prob_X_less <- pbinom(4, 10, .6)
prob_Y_less <- pbinom(4, 10, .7)
prob_X_less + prob_Y_less - prob_X_less * prob_Y_less


#### Gesetze für Zufallsvariablen
# Zufallsvariable:  Zuordnungsvorschrift, die jedem möglichen Ergebnis eines Zufallsexperiments eine Zahl zuordnet

### Multiplikation von ZV mit Konstante 
# X ~ Binomial(10,.5)
# Y ~ 3 * X
# => X=2 -> Y=6, X=3 -> Y=9 
# ==> Was sind die Eigenschaften von Y: z.B. Erwartungswert und Varianz
# Regeln sind unabhängig von der Verteilung(!!)

X <- rbinom(100000, 10, .5)
Y <- X * 3

## Erwartungswert 
# E[k * X] = k * E[X]
mean(X) ; mean(Y)

## Varianz 
# Var[k * X] = k^2 * Var[X]
# => unabhängig von der Verteilung (hier: binomial)
var(X); var(Y)


### Kombination von Zufallsvariablen
# X ~ Binomial(10,.5)
# Y ~ Binomial(100,.5)
# Z ~ X + Y
# => Z folgt zwar nicht einer Binomialverteilung, 
#    aber man kann trotzdem etwas die Eigenschaften sagen
X <- rbinom(100000, 10, .5)
Y <- rbinom(100000, 100, .2)
Z <- X + Y

## Erwartungwert
# E[X+Y] = E[X] + E[Y]
# => selbst wenn X und Y nicht(!) unabhängig
mean(X); mean(Y) ; mean(Z)

## Varianz
# Var[X+Y] = Var[X] + Var[Y]
# => nur(!) wenn X und Y unahängig 
var(X); var(Y) ; var(Z)

#### Bayessche Statistik 
### Updating des Beliefs mit Evidenz

### Updating des beliefs nachdem man Evidence gesehen hat
# z.B. bekommt Münze und hat skeptischen Belief: 50% Wahrscheinlich, dass Münze fair
#      nachdem man 20 mal wirft und 14 mal Kopf bekommt => updaten des beliefs
# Wobei: fair=.5, biased = .75
# => zu bestimmen: 
#    1. Ist münze fair oder biased? 
#    2. Mit welcher Wahrscheinlichkeit ist Münze fair / biased

### Updating des Beliefs durch Simulation
# Anahme: 50% Wahrscheinlichkeit, dass Münze fair 
# betrachten als 2 gleichgroße Haufen von Münzen - fair und unfair 
# => gleichgross, weil belief: zu 50% Wahrscheinlichkeit ist Münze fair
# => jede wird 20 mal geworfen 
set.seed(0)
fair <- rbinom(50000, 20, .5)
(fair_14_sum <- sum(fair == 14)) # 1815
biased <- rbinom(50000, 20, .75)
(biased_14_sum <- sum(biased == 14)) # 8405
hist(fair) ; hist(biased) # => vergleich der Bars mit 14
(total_14_heads = fair_14_sum + biased_14_sum) 
# => Anzahl aller Versuche mit 14 mal Kopf (fair und biased)
# => nun kann man die bedingte Wahrscheinlichkeit berechnen,
#    dass Münzed biased unter der Bedingung, dass man 14 mal Kopf bekommt

## Bedingte Wahrscheinlichkeit
# Pr(Biased | 14 Kopf) = (Anzahl bias mit 14 Kopf) / (Anzahl gesamt 14 Kopf)
# Wahrscheinlichkeit, dass Münze biased nachdem 14 mal Kopf beobachtet
(cond_prob_biased <- biased_14_sum / total_14_heads)
# => 82%

## A-posteriori-Wahrscheinlichkeit
# Posterior probability
# = Belief nach der Beobachtung => nach updating des Beliefs
# A-priori = Belief vorher: 
#      50% wahrscheinlichkeit, dass münze biased
# A-posteriori = Belief nachdem 14xKopf beobachtet: 
#      82% wahrscheinlichkeit, dass münze biased


#### A-priori-Wahrscheinlichkeit
# (Prior Probabilty)
# Wahrscheinlichkeitswert, der aufgrund von Vorwissen bestimmt wird
# Laplace: Sofern es keinen expliziten Grund gibt, etwas anderes anzunehmen, 
#          wird allen elementaren Ereignissen dieselbe Wahrscheinlichkeit zugeordnet
#          => Indifferenzprinzip
# Vohriges Beispiel: 
# Skeptischer Belief: Münze ist nur zu einer Wahrscheinlichkeit von 50% fair
# => gleichgrosse haufen
# Gutgläubiger Belief: Münze ist nur zu einer Wahrscheinlichkeit von 90% fair
# => Verhältnis der größen der der Haufen 90:10
set.seed(0)
fair <- rbinom(90000, 20, .5)          # => größerer haufen
biased <- rbinom(10000, 20, .75)        # => Kleinerer Haufen
(fair_14_sum <- sum(fair == 14))       # 3318
(biased_14_sum <- sum(biased == 14))   # 358
hist(fair); hist(biased) # Vergleich der Höhen der Bars bei 14

# Bedingte Wahrscheinlichkeit berechnen, dass Münze biased
(total_14_heads = fair_14_sum + biased_14_sum) 
(cond_prob_biased <- biased_14_sum / total_14_heads)
# => 33% => A-posteriori-Wahrscheinlichkeit

## A-posteriori-Wahrscheinlichkeit
# = Belief nach der Beobachtung => nach updating des Beliefs
# A-priori = Belief vorher: 
#      10% wahrscheinlichkeit, dass münze biased
# A-posteriori = Belief nachdem 14xKopf beobachtet: 
#      33% wahrscheinlichkeit, dass münze biased


#### Verschiedene Wahrscheinlichkeiten für Kopf
# Anstatt fair(.5) oder nicht fair(0.75) nun 3 Mögliche Münzarten:
# Faire Münze: 50% Kopf, 
# Negativer Bias: 25% Kopf
# Positiver Bias 75% Kopf
# Wahrscheinlichkeit für Münzart: 80% fair, 10% negativer Bias, 10% positiver Bias
# Beobachtet: 14/20 Kopf => Ist Münze fair?

# 80000 Versuche mit fair coin, jweils 10000 von positiv / negativ
flips_fair <- rbinom(80000, 20, .5)
flips_negative <- rbinom(10000, 20, .75)
flips_positive <- rbinom(10000, 20, .25)

# Anzahl der Münzen mit 14 mal Kopf in den Haufen
fair_14 <- sum(flips_fair == 14)
negative_14 <- sum(flips_negative == 14)
positive_14 <- sum(flips_positive == 14)

# A-posteriori-Wahrscheinlichkeit berechnen, dass Münze fair
(cond_prob_fair <- fair_14 / (fair_14 + positive_14 + negative_14))
# => 69%


##### Updating mit Bayes Theorem
# Voher: Bestimmung der bedingten Wahrscheinlichkeit durch Simulation
#  => densities wurden durch simulation mittels vieler Versuche künstlich generiert 
#  => Histogram: wieviele der 10000 Versuche endeten mit 14 mal Kopf => entspricht der density am punkt ()
# Jetzt: Wahrscheinlichkeitsdichte verwenden

## Wahrscheinlichkeit, dass coin biased bei 14 mal Kopf, prior = 0.9
# Simulation
set.seed(0)
fair <- rbinom(90000, 20, .5)
biased <- rbinom(10000, 20, .75)
(sum_fair_14 <- sum(fair == 14)) # 3318
(sum_biased_14 <- sum(biased == 14)) # 346
(cond_prob <- sum_biased_14 / (sum_fair_14 + sum_biased_14))

## Über Wahrscheinlichkeitsdichte
# Wahrscheinlichkeit, dass Coin fair UND 14 mal Kopf beobachtet
# ---- Pr(14 Kopf | Fair) * Pr(Fair)
dbinom(14, 20, .5) * .9 # 0.03326797
# => Multiplikation mit A-prior Wahrscheinlichkeit, dass Coin fair
#    (Entspricht 90000 in rbinom)

# --- Pr(14 Heads∣Biased) * Pr(Biased)
dbinom(14, 20, .75)  * .1

# Bayes Theorm
# Pr(A∣B) = Pr(B∣A) * Pr(A) / Pr(B∣A) * Pr(A) + Pr(B∣not A) * Pr(not A)
# A = Biased
# B = 14 Kopf

prob_14_fair <- dbinom(14, 20, .5) * .9
prob_14_biased <- dbinom(14, 20, .75) * .1
prob_14_biased / (prob_14_fair + prob_14_biased)
(dbinom(14, 20, .5) / (dbinom(14, 20, .5) + dbinom(14, 20, .75)))
# Pr(Biased∣14 Kopf) = Pr(14 Heads and Biased) / (Pr(14 Kopf und Fair) + Pr(14 Kopf and Biased))
# = (Pr(14 Kopf∣Biased) * Pr(Biased)) / (Pr(14 Kopf∣Biased) * Pr(Biased) + Pr(14 Kopf∣Fair) * Pr(Fair))


## Weitere Beispiele
# Belief: mit 50% Wahrscheinlichkeit fair
dbinom(14, 20, .5) / (dbinom(14, 20, .5) + dbinom(14, 20, .75))
dbinom(18, 20, .5) / (dbinom(18, 20, .5) + dbinom(18, 20, .75))
# 99% Belief, dass Coin fair
probability_16_fair <- dbinom(16, 20, .5)
probability_16_biased <- dbinom(16, 20, .75)
(probability_16_fair * .99) / (probability_16_fair * .99 + probability_16_biased * .01)


#### Normalverteilung
# X ∼ Normal(,σ)
# μ = σ = sqrt(Var(X))


### Normal-Approximation
# Methode um Binomialverteilung für große Stichproben durch Normalverteilung anzunähern
# => Anwendung des Satzes von Moivre-Laplace => somit auch Anwendung des Zentralen Grenzwertsatzes 
# => biomialverteilung nähert normal an für größere size
# μ = size * p
# σ = sqrt(size * p * (1 - p)

binomial <- rbinom(100000, 1000, .5)
# Gemäß Regeln von oben
expected_value <- 1000 * .5
variance <- 1000 * .5 * (1 - .5)
stdev <- sqrt(variance)

# Approximation erstellen
normal <- rnorm(100000, expected_value, stdev)
# QQ Plot zum Verleich
qqplot(binomial,normal)


### Poison Verteilung
# Zur Modelierung von seltenen Events als Counts innerhalb eines Intervals
# (und man sich nicht für gesamtanzahl interessiert wie bei der Binomialverteilung)
# z.b. Wieviele Kunden kommen pro Stunde 
# => technisch zwar auch eine Prozent-verhältnis -  teil eines ganzen (alle Leute),
#    aber man denkt so nicht darüber nach oder interessiert sich nicht dafür
# Nähert Binomial für gerine Wahrscheinlichkeit an => ist aber einfacher
# X ∼ Poisson(λ)
# E[X]   = λ
# Var(X) = λ
poisson <- rpois(100000, 1)
hist(poisson)

## Dichte
# Simulieren von 100000 Versuchen von Poison(2)
poisson_sample <- rpois(100000, 2)
# Prozent mit Wert = 0 
mean(poisson_sample == 0)
# Berechnung mit Dichtefunktion
dpois(0, 2)

## Addtion von zwei Poison Variablen => Poison Variable
X <- rpois(100000, 1)
Y <- rpois(100000, 2)
Z1 <- X + Y
Z2 <- rpois(100000, 3)
hist(Z1)
hist(Z2)

## Nähert Binomialverteilung mit sehr geringer Wahrscheinlichkeitan
# => wie häufig tritt ein seltenes Ereignis in einer großer Menge von Möglichkeiten auf
# Entspricht viele Münzen, mit einer sehr geringen Wahrscheinlichkeit, zu werfen
binomial <- rbinom(n=100000, size=1000, p= 1 / 1000)
hist(binomial) # => nicht symmetrisch

### Geometrische Verteilung
# Anzahl der Versuche bis Ereignis eintritt
# z.B. Gerät hat jeden Tag 10% Wahrscheinlichkeit kaputt zu gehen 
# => wieviel tage sind zu erwarten bis es kapput ist
# X ∼ Geom(p)
# E[X] = 1/p - 1

# Simulation mit rbinom
flips <- rbinom(100, 1, .1)
which(flips == 1)[1]
replicate(10, which(rbinom(100, 1, .1) == 1)[1])

# Simulation mit rgeom
(geom <- rgeom(100000, .1))
mean(geom)
hist(geom)

### CDF
# Wahrscheinlichkeit, dass Maschine am 5 Tag oder früher kaputt geht
pgeom(4, .1)
# Wahrscheinlichkeit, dass auch nach 20 tagen noch funktionstüchtig
1 - pgeom(19, .1)
# Funktiontüchtig am Tag 1-30
still_working <- 1 - pgeom(0:29, .1)
qplot(1:30, still_working)
