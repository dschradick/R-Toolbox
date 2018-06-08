########## CORREPONDENCE ANALYSIS
# Analyse der Assoziation zwischen zwei kategorischen Variablen,
# indem die Kontingenztabeln dieser Variablen analysiert werden.
# => Erweiterung für PCA zur Analyse von kategorischen Variablen
# MCA = Multiple Correspondence Analysis
library(factoextra)
library(FactoMineR)
library(gplots)
data(housetasks)

head(housetasks) # Was wird von wem im Haushalt gemacht

#### Daten anzeigen - GUT!
dt <- as.table(as.matrix(housetasks))
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

## Assoziation zwischen Reihen- und Spalten-Variable?!
chisq <- chisq.test(housetasks)
chisq # => ja

#### Correspondance Analysis
res.ca <- CA(housetasks, graph = T)

## Spree-Plot
fviz_eig(res.ca) 


## Bi-Plot
# - Codierung: Reihen = blau, Spalten = rot
# - Distanz zwischen Reihen und Spalten Punkten = Maß der Ähnlichkeit
# - Reihenpunkte mit ähnlichem Profile sind dicht zusammen 
# - Spaltenpunkte analog
fviz_ca_biplot(res.ca, repel = TRUE)

# Relation von Reihen-Punkten
fviz_ca_row(res.ca, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)
# - Reihen mit ähnlichem Profil dicht zusammen
# - Negativ korrelierte Reihen auf anderen Seite vom Ursprung (gegenüberliegender Quadrant)
# - Distanz zwischen Reihenpunkt und Ursprung = Mass der Qualität der Repräsentation

corrplot(row$cos2)

## Qulität der Repräsentation
fviz_cos2(res.ca, choice = "row", axes = 1:2)

## Eigenwerte ausgeben
eig.val <- get_eigenvalue(res.ca)
eig.val
## Ergebnisse für Reihenvariablen
res.row <- get_ca_row(res.ca)
res.row$coord          # Koordinaten
res.row$contrib        # Beitrag zu PCs
res.row$cos2           # Qulität der Repräsentation
## Erbnisse für Spaltevariablen
res.col <- get_ca_col(res.ca)
res.col$coord          # Koordinaten
res.col$contrib        # Beitrag zu PCs
res.col$cos2           # Qulität der Repräsentation

library("corrplot")
fviz_ca_row(res.ca, alpha.row="cos2")
corrplot(row$cos2, is.corr=FALSE)

# alternativ mit MASS package: 
library(MASS)
res.ca <- corresp(housetasks, nf = 3)
res.ca

