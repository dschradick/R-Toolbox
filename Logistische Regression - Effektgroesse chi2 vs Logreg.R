library(gmodels)

#### Für 2x2 Tabellen 
# 2x2 => bei größer Interpretation schwierig
#
# Effektgröße: Odds-Ratio 
#
# Mittels: 
# 1. chi-squared + Odds-Ratio aus Kontingenztafel berechnen
# 2. Logistische Regression 
# => liefern äquivalentes Ergebnis
#
# eg. CTR in AB-Test
# => Effektgröße: Odds-Ratio 
#    für Assoziation von 2 kategorischen Variablen mit zwei Leveln
#


### Normaler chi-squared test
chisq.test(mtcars$am, mtcars$vs)
# => test ob beziehung besteht 
#    In diesem Fall nicht, weil sample klein 
#    aber man sollte trotzdem die Größe des Effekts betrachten
# => Effektgröße durch manuelles berechnen des Odds-Ratio aus 


CrossTable(mtcars$am, mtcars$vs, 
           chisq = T,      # normaler chi-squared test
           expected = T,   # expected counts für zellen
           fisher = T,     # Fisher Exact Test => incl. conf-int)
           sresid = T)     # Standardized Residuals !!!!
                           #  => um den Beitrag zur Chi-Squared zu sehen
                           #     achte auf |sresid| > 1.96

### Odds-Ratio als Effect size 

## 1. Manuelle Berechnung der Effektgröße mittels Kontingenztafel 
# Odds berechnen - aus Tabelle
(odds_of_am_no_vs   <- 12 / 6)    #  = 2
(odds_of_am_with_vs <- 7  / 7 )   #  = 1

(odds_ratio <- odds_of_am_no_vs /  odds_of_am_with_vs) 
#OR = 2 : 1  => 2 / 1 = 2

   
## 2. Berechnung der Effektgröße mittels logistischer Regression
mod <- glm(am ~ vs, data=mtcars, family=binomial)
exp(coef(mod))
# OR: 2
# => LR erlaubt aber andere Faktoren zu kontrollieren



