########## VARIABILITÄT VON METRIKEN
# Beispiel: Konfidenz-Intervall für Retention
# => In welchem Bereich liegt wahrscheinlich der tatsächliche Wert?
# Bsp: 10 Test-Spieler: D1 retained: 3
prop.test(x = 3,    n = 10,    correct = FALSE)   # ~10% - 60%
prop.test(x = 30,   n = 100,   correct = FALSE)   # ~22% - 39%
prop.test(x = 300,  n = 1000,  correct = FALSE)   # ~27% - 33%
prop.test(x = 3000, n = 10000, correct = FALSE)   # ~29% - 31%


# Conversion
# 5% Conversion
prop.test(x = 5,    n = 100,   correct = FALSE)   # ~2.1% - 11.1%
prop.test(x = 50,   n = 1000,  correct = FALSE)   # ~3.8% -  6.5%
prop.test(x = 500,  n = 10000, correct = FALSE)   # ~4.6% -  5.4%


# Conversion 
# Vorher: 400/10000 => 4%
# Jetzt: 45/1000 => +0.5%?
# => wieviele Samples?
# http://ww2.coastal.edu/kingw/statistics/R-tutorials/proport.html
library(stats)
prop.test(x=c(45,400), n=c(1000,10000), alternative="greater", conf.level=.95)
# => estimate einsetzen
power.prop.test(p1=.045, p2=.04, sig.level=.05, power=.8, alternative="one.sided")
# => 20126 pro gruppe
power.prop.test(p1=.050, p2=.04, sig.level=.05, power=.8, alternative="one.sided")
# => 5312 pro gruppe
