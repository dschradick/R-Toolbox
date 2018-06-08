########## SEM & CFA
# Strukturprüfendes konfirmatorisches multivariates Verfahren
# Ähnlich normaler Regression 
# => aber abhängige Variable kann in anderen Gleichungen als unabhänige benutzt werden
# Schätzt und testet korrelative Zusammenhänge zwischen abhängigen  
# und unabhängigen Variablen und den verborgenen Strukturen dazwischen.
# Überprüft, ob die für Modell angenommenen Hypothesen mit den gegebenen Variablen übereinstimmen

# Strukturellen Gleichungen ~~ kausale Zusammenhänge zwischen Variablen
# => allerdings wie bei regression auch nicht notwendigerweise kausal
# https://www.r-bloggers.com/structural-equation-modelling-in-r-part-1/
# https://www.r-bloggers.com/structural-equation-modelling-in-r-part-2/

library(htmlTable)
library(semPlot)
library(semTools)
library(nonnest2)
library(htmlTable)

## Confirmatory Factor Analysis 
# Beispiel: 9 testscore in 3 kategorien 
# => allgemeine performance in Kategorie ist latente Variable
head(HolzingerSwineford1939) 
#?HolzingerSwineford1939  

HS.model <- ' visual  =~ x1 + x2 + x3      
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9'
# => visual, textual, speed sind die latenten variablen 

fit <- cfa(HS.model, data = HolzingerSwineford1939)

summary(fit, fit.measures=FALSE)
# Konfidenz-Intervalle für Koeffizienten
parameterEstimates(fit, ci = TRUE, level = 0.95)

## Bestimmen von Goodness of Fit
# Ideale Werte
# - Chi-squared: > 2df
# - RMSE Adjusted:  < 0,05
# - SMSR: < 0,1
# - GFI: > 0,9
fitMeasures(fit, c("chisq", "rmsea", "srmr", "gfi", "ecvi"))


reliability(fit)

# lavaan output als table
htmlTable(txtRound(parameterEstimates(fit, ci = TRUE, level = 0.95), digits=3, excl.cols=1), align="l|r|r|r|r|r|r|r|r|r")
htmlTable(txtRound(reliability(fit),3), align="l|r|r|r|r")


semPaths(fit, style="lisrel", 
         whatLabels = "std", edge.label.cex = .6, node.label.cex = .6, 
         label.prop=0.9, edge.label.color = "black", rotation = 4, 
         equalizeManifests = FALSE, optimizeLatRes = TRUE, node.width = 1.5, 
         edge.width = 0.5, shapeMan = "rectangle", shapeLat = "ellipse", 
         shapeInt = "triangle", sizeMan = 4, sizeInt = 2, sizeLat = 4, 
         curve=2, unCol = "#070b8c")
