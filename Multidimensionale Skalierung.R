########## MULTIDIMENSIONALE SKALIERUNG
# Multivariater Ansatz zur Visualisierung der similarity/dissimilarity zwischen Samples 
# und plotting der Punkte im k-dimensionalen Raum (k kann frei gewählt werden)
# => Je weiter voneinander entfernt, desto unähnlicher
# 
# Eingabe:      Dissimilarity matrix = repräsentiert Distanzen zwischen Paaren von Objekten
# Konfiguration: Lösung der MDS - meist 2 oder 3 Dimensionen wg Interpretierbarkeit
# => Berechnet wie PCA die Eigenwerte und Eigenvektoren

#### Klassisches MDS
# Behält die originale Distanz zwischen Punkten möglichst bei
# => gefittete und originale Distanzen sind in selber Metrik
# auch: Principal coordinates(!) analysis
# N Zeilen (Observations) x p Spalten (Variablen)
# dist erzeugt die Distance-Matrix des Datensatz
# => kann verschiedenen Metriken verwenden: default method = "euclidean" (geht auch 'manhatten)

## MDS durchführen
d <- dist(mtcars)                # Dissimilarity Matrix berechnen
dist(mtcars[1:10,1:5])           # zeigt die dissimilarity matrix für einfacheren Fall an => angucken gut zur Überprüfung
fit <- cmdscale(d,eig=TRUE, k=2) # k Anzahl der Dimensionen
fit 

## Visualisieren
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Koordinate 1", ylab="Koordinate 2", main="Metrische	MDS",	type="n")
text(x, y, labels = row.names(mtcars), cex=.7)

## Besser
library(dplyr)
library(ggpubr)
mds <- mtcars %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()

colnames(mds) <- c("Dim.1", "Dim.2")
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(mtcars),
          size = 1,
          repel = TRUE)



#### Non-metric multidimensional scaling
# verwendet rank-order dissimilarity 
# => metrik des distanz-wertes unerheblich, sondern nur der Wert in Relation zu den Distanzen zwischen anderen Paaren
# Zwei Versionen in MASS Package
# isoMDS(): Kruskal’s non-metric multidimensional scaling
# sammon(): sammon’s non-linear mapping 
library(MASS)
d <- dist(mtcars)
fit <- isoMDS(d, k=2) 


