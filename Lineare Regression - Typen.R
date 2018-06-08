########## LINEARE REGRESSION - MODELL-TYPEN
library(dplyr)
library(ggplot2)
library(broom)
library(plotly)
library(modelr)
library(RColorBrewer)
library(plotly)
library(Stat2Data)
library(openintro)

#### Daten vorbereiten
# Mario-Kart ebay auktionen; 
# wheels = anzahl lenkrad-kontroller
mario_kart <- marioKart %>% filter(totalPr < 100)
head(mario_kart)



#### Einfache lineare Regression
# Zu modellieren:  totalPr = β0 + β1 * wheels
slr.mod <- lm(mario_kart, formula = totalPr ~ wheels)
summary(slr.mod) 



#### Parallel Slopes Model
# = Regression mit einer kategorischen Variable
# Erlaubt es bzgl. einer Variable zu kontrollieren 
# Parallel Slopes Model: lm(data, formula = y ~ x + z), 
#                        wobei y = numerische, z = katogorische expanatory
# Zu modellieren:  totalPr = β0 + β1 * wheels + β2 * is_used
mod <- lm(mario_kart, formula = totalPr ~ wheels + cond)
summary(mod)
coef(mod) # =>  wheels  = 7.233; condused -5.585 

## Resulierendes Modell: 
# Für cond = used: totalPr =  42.370 − 7.233 * wheels + 5.585 * 1 
# Für cond = new:  totalPr =  42.370 − 7.233 * wheels + 5.585 * 0
# => Wenn Geraden eng beinander, so ist Koeffizient der kategorischen sehr klein 
#    und  Effekt der Variable sehr gring und Modell sehr ähnlich zum einfachen Regressionsmodell
ggplot(data = augment(mod), aes(x = wheels, y = totalPr, color = cond)) + 
  geom_point() +
  geom_line(aes(y = .fitted, color = cond)) 

## Modell Interpretation
# Koeffizient besagt wie response mit der jeweiligen abhängigen Variable ansteigt
#    nachdem die anderen Variable kontrolliert wurden (dh. unabhängig vom Wert der anderen)
# 1. Numerisch: 
#     Besagt wie die Response-Variable mit der numerischen Var ansteigt 
#     nachdem für die kategorische kontrolliert wurde
#     => Also unabhängig davon ob die kategorische den einen oder anderen Wert hat
#     BSP: Für jeden zusätzlichen Reifen, steigt der Preis um 7,23 
#          unabhängig davon ob, ob das Spiel  alt oder neu ist
# 2. Kategorisch: Besagt wie die Response Variable sich verhält, je 
#                nachdem welchen Wert die Kategorische hat - unabhängig von der numerischen
#    BSP: Bezeichung von R als condused gewählt:
#         Dh. Wenn Cond = used, dann ist der Peis (unabh. von num.) $5,58 geringer 
#             im Gegensatz zu new




#### Modelle mit (kategorischen) Interaction Terms 
# Paralell Slopes Geraden darauf beschränkt parallel zu sein
# Aber Verhältnis der abhängigen Variable von der numerischen unabhängigen 
# ist verschieden für den jeweiligen Wert der kategorischen Variable
# => Slope verändert sich abhängig von der kategorischen Variable
# => verschiedene Regressionsgerade für die jeweilige Werte der kategorischen Variable

# Durch Einführung von Interaction Term (x2 = kategorische)
# Vorher:           y = β0 + β1 * x1 + β2 * x2
# Mit Interaction:  y = β0 + β1 * x1 + β2 * x2  + β3 * x2 * x3
# Dann: Für x2 = 0: y = β0 + β1 * x1                  (Erste Gerade)
#           x2 = 1: y = (β0 + β2) + (β1 + β3) * x1    (Zweite Gerade) 
#                                                     (β1,β3 waren beides faktoren von x1)
#                                   
# lm(y ~ x + z + x:z, data = mydata)
lm(mario_kart, formula = totalPr ~  duration + cond + duration:cond)

## Visualisieren
ggplot(mario_kart,aes(x=duration,y=totalPr,color=cond)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

## Simpons Paradox auch bei diesen Daten => aufgehoben
(slr <- ggplot(mario_kart, aes(y = totalPr, x = duration)) + geom_point() + 
    geom_smooth(method = "lm", se = 0)) # eine gerade 
slr + aes(color = cond) # zwei Geraden




#### Multiple Regression
# zu modellieren: y = β0 + β1 * x1 + β2 * x2 (...)
# R:  lm(y ~ x1 + x2, data = data)
mod <- lm(mario_kart,formula = totalPr ~ duration + startPr)

## Visualieren mittels Tiling
grid <- mario_kart %>% data_grid(
  duration = seq_range(duration, by = 1),
  startPr = seq_range(startPr, by = 1) )
mod <- lm(mario_kart,formula = totalPr ~ duration + startPr)
price_hats <- augment(mod, newdata = grid)

(p <- ggplot(mario_kart,aes(x=duration,y=startPr)) +
    geom_point(aes(color = totalPr)) +
    geom_tile(data = price_hats, aes(fill = .fitted), alpha = 0.7))
# => zeigt den Verlauf des Preis - je höher & links desto teuerer

## Interpretation der Koeffizienten
# Koeffizient quantifiziert den Effekt bei Veränderng der entsprechenden Variable
# auf die abhängige Variable, wenn wenn die anderen Variablen konstant gehalten / kontrolliert werden
# alternativ: wenn die anderen Variablen kontrolliert werden
# Bsp:

coef(mod)     # duration = -1.508, startPr = 0.233

# => höheren Preis für Auktionen mit geringer Duration und höherem Startpreis
#    Reflektiert in der Rate der Änderung im Fabrverlauf
#
# Wie hoch ist die Änderung
# 1. duration
#    für gegebenen Startpreis (z.B startpreis = 20) sinkt 
#    der erwartet Verkaufspreis um $1.50 pro Tag 
#   "Für jeden zusätzlichen Tag den die Auktion dauert, fällt der erwartete Preis um $1.51,
#    nachdem für die Variable startpr kontrolliert wurde"
p + geom_hline(yintercept=20, color = 'red') 
# => Rate entspricht der horizontalen Veränderung der Farbe entlang Wert = 20 bestimmten Geraden

# 2. startPr
#    für eine gegebene Duration (z.B. Duration = 5 Tage) steigt 
#    erwartete Verkaufspreis um $0.23 pro Dollar (des startpreis)
p + geom_vline(xintercept=5, color = 'red')
# => Rate entspricht der verikalen Veränderung der Farbe für Wert 5

#### Verleich der Koeffizienten
# !! Können nicht direkt verglichen werden, wenn sie verschiedene Einheiten oder Skalen benutzen
# => größerer Koeffizient bedeutet nicht, dass er wichtiger ist
#  Bsp: Bei Mariokart haben die Koeffizienten verschiedene Einheiten
#  1: Duration: Dollar pro Tag &  2: StartPr:  Dollar pro Dollar
#  => Nicht direkt vergleichbar!




## Höhere Dimensionen
# Erzeugen von Modell mit 2 parallelen Ebenen
lm(formula = totalPr ~ duration + startPr + cond, data = mario_kart)
# => condused = -8.9493 = Abstand zwischen den beiden Ebenen
#    = der zu erwartetende Aufpreis für ein neues (relativ zum altem) Mariokart ist $8,95   
#      wenn für die Variablen duration und startpr kontrolliert wird

## Alle Variablen mit einbeziehen  
lm(formula = totalPr ~ duration + startPr + cond + wheels + nBids,  data = mario_kart)

# Bsp Interpretation: wheels = 6.7216
# Jedes zusätzliche Wheel erhöht den erwarteten Verkaufspreis um $6.72, 
# wenn für die Variablen duration, startPr, cond und nBids kontrolliert wird.

# Alle Variablen ausser nbids
lm(formula = totalPr ~ . - nBids,  data = mario_kart)