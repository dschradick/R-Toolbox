########## STATISTISCHE TESTS - PROPORTION

#### One-Proportion test
# (Test von Anteilen, 1 Stichprobe)
# Vergleicht erwarteten Anteil mit einer theoretischen / erwarteten

# binom.test() => exact binomial test - benutzen wenn Sample size klein
# prop.test()  => wenn sample size gross( N > 30)
# Bsp: Population mit 50/50 männlich weiblich -> 160 bekommen krebs, davon 95 männnlich, 65 weiblich
# => sind männer stärker von krebs betroffen?
res <- prop.test(x = 95, n = 160, p = 0.5, correct = FALSE) # correct = Yates continuity correction
res
res$p ; res$conf.int

#### Two-Proportion test
# (Test von Anteilen, 2 Stichprobe)
# Vergleicht zwei beobachtete Anteilen 
# Bsp: Ist der Anteil an raucher in beiden Gruppen die gleiche?
res <- prop.test(x = c(250, 230), n = c(500, 500))
res


#### Chi-square goodness of fit test
# Vergleicht beobachtete Verteilung mit erwartetere Verteilung in der man zwei oder mehrere Kategorien hat
# => Vergleicht mehrere beobachtete Anteil mit erwarteter Wahrscheinlichkeit
# H0: kein signifikanter Unterschied zwischen b 
# Bsp: Alle Farben sollten bei Planze gleichvertreten sien
tulip <- c(81, 50, 27)
# x =  numerischer vektor; p = vektor mit wahrscheinlichkeiten derselben länge wie x
res <- chisq.test(tulip, p = c(1/3, 1/3, 1/3))
res # => < 0.05 =>  Verteilungen sind unterschiedlich


#### Chi-square test of independence
# Analysiert Kontingenz-Tafel, bestehtend aus zwei Kategorischen
# => bewertet ob signifikante Assoziation zwischen zwei kategorischen Variablen besteht
# H0: Reihen und Spalte Variable sind unabhänig 
library(factoextra); library(gplots) ; library(graphics); library(corrplot); data(housetasks)

## Darstellung
dt <- as.table(as.matrix(housetasks))
# Balloonplot
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="", label = FALSE, show.margins = FALSE)
# Mosaicplot
mosaicplot(dt, shade = TRUE, las=2, main = "housetasks")

## Test
chisq <- chisq.test(housetasks)
chisq # p < 0.04 => sind abhängig
# X-squared = 1944.5 

## Aus Resultat können der observed und expected count extrahiert werden
chisq$observed
round(chisq$expected,0)

## Pearson residuals für jede Zelle
# => wieviel trägt sie zum gesamten chi-squared Wert(1944.5) bei
round(chisq$residuals, 3)
# als prozent
contrib <- 100 * chisq$residuals^2 / chisq$statistic
round(contrib, 3)
# Visualisieren!!
# Blau / rot = positive / negative residuals 
corrplot(chisq$residuals, is.cor = FALSE)
# => Stärkste Assoziation => Husband & Repair

## G-Test = Likelihood ratio test
library(DescTools)
library(RVAideMemoire)

