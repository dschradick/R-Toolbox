########## CLUSTERING 
library(GMD)
library(factoextra)


#### SKALIEREN NICHT VERGESSEN!!!

#### Müssend Daten skaliert werden?! 
## Mittelwert & SD Überprüfen...
colMeans(USArrests)
apply(USArrests, 2, sd)
df <- scale(USArrests)

#### Vergleich kmeans mit hclust
#hclust.cut <- cutree(hclust.model, k = 3)
#table(kmeans.model$cluster, hclust.cut)

#### ZUERST: Einfache Heatmap - zuerst immer!
# pheatmap ist speziell für clustering - erlaubt auch annotations
library(pheatmap)
pheatmap(scale(df), cutree_rows = 3, cutree_cols = 3) 
# => Zahlen ausprobieren und schauen wo es sinn macht
# Erlaubt es 2 Clusterings zu überlagern und über Annotations aufzuschlüssen!!
# Vorher Sampling, so dass alle Gruppen mit Observations vorhanden sind (über subsampling)
df <- Filter(!is.numeric, mtcars)
# Eigenschaften der Reihe eigenen sich gut Features
# => erlaubt dann die Verteilung des Features in den Clustern zu sehen
# => Erlaubt es auch Eigenschaften mit einzubeziehen die nicht im Clustering berücksicht worden sind
#        z.B. Premium-User Flag
# => Man sieht WELCHES FEATURE den Cluster bestimmt - welche Werte davon enthalten sind - wie rein die diesbezüglich sind
#           Validierungs-Variablen können benutzt werden => wenn stimmt, dann kann man sehen welche features zusammenhängen
# Könnte auch als Ziel die Reinheit bzgl einer Variable angeben - danach rückschlüsse auf Variablen
# Frage: Welche Feature werden sich wohl clustern?!
annotation_row = data.frame(
  Cyl = mtcars$cyl,
  Am =  mtcars$am,
  hp =  mtcars$hp
)
length(colnames(df))

## Annotation für Spalten müssen Eigenschaften des Features sein
colnames(df) # => Elemente in annotation_col müssen genauso lang sein
annotation_col = data.frame(
  Type = c('Efficiency','Engine','Engine','Technical','Luxury','Acceleration','Acceleration','Luxury','Engine')
)


rownames(annotation_row) <- row.names(df)
rownames(annotation_col) <- names(df)

pheatmap(scale(df), 
         annotation_row =annotation_row, 
         annotation_col = annotation_col, 
         cutree_rows = 4, cutree_cols = 4)



#### Test-Daten erzeugen
# => stellen tatsächliche cluster da
set.seed(0)
x <- matrix(rnorm(50*2), ncol=2)
x[1:25,1] <- x[1:25,1]+3
x[1:25,2] <- x[1:25,2]-4


#### Distanz Matrix visualisieren
dist.eucl <- dist(x, method = "euclidean")
round(as.matrix(dist.eucl)[1:3, 1:3], 1)
fviz_dist(dist.eucl,order = F)


#### K-MEANS
## k-Means clusting durchführen
# hier: 2 cluster
# nstart = anzahl wie häufig kmeans wiederholt wird
# => bestes outcome mit minimalen total within cluster sum of squares ausgewählt
#  => sollte hoch sein (z.B. 20,50), da sonst ungewünschtes lokales Optimum erreicht wird
km <- kmeans(x,3,nstart=20)
plot(x, col = km$cluster,
     main = "k-means with 3 clusters", 
     xlab = "", ylab = "")
#plotcluster(km,k$cluster)

## Erebnis anzeigen
km
km$cluster # => welchem Cluster sind datenpunkte zugeordnet
plot(x, col=(km$cluster+1), main="K-Means mit K=2", xlab="", ylab="", pch=20, cex=2)
# => perfekte trennung bei k=2 - vs k=3...

## k-Means mit 3 clustern
set.seed(0)
km <- kmeans(x,3,nstart=20)
plot(x, col=(km.out$cluster+1), main="K-Means mit K=3", xlab="", ylab="", pch=20, cex=2)
# => ungeeignet

### tot.withinss = total within-cluster sum of squares
# => wird versucht zu minimieren
# withinss = individuellen within-cluster sum of squares
# ..bei verschiedenen nstart values
set.seed(0)
km=kmeans(x,3,nstart=1)
km$tot.withinss
km=kmeans(x,3,nstart=20)
km$tot.withinss
km$withinss

### Scree-Plot
# zur Bestimmung der Anzahl von Clustern => im Ellbogen
mss <- c()
par(mfrow=c(1,1))
for (i in 2:15) mss[i] <- sum(kmeans(x,centers=i)$withinss)
plot(1:15, mss, type="b", xlab="Anzahl von Clustern", ylab="Within groups sum of squares")

#### HIERARISCHES CLUSTERING
# => Wenn Anzahl der Cluster nicht  
## Durch hclust implementiert

# Die 4 Arten die Distanz zwsichen den Clustern zu bestimmen: 
# Complete: Maximale intercluster dissimilarity
# Single: Minimale intercluster dissimilarity
# Average: Mittlere intercluster dissimilarity
# Centroid: Dissimilarity zwischen den Cluster centroids
# => complete und average am häufigsten und machen idR die balanciertesten Bäume
# dist() um die euklidische distanz zwischen den Observerations zu berechnen
hc.complete  <- hclust(dist(x), method="complete")
hc.average <- hclust(dist(x), method="average")
hc.single <- hclust(dist(x), method="single")

## Struktur des resultierenden Modells
summary(hc.complete)

## Visualisieren als Dendogramme
# !!!! 
# Schlussfolgerung bezüglich der Ähnlichkeit von zwei Observations 
# basiert NICHT auf der horizontalen Nähe im Dendogram 
# SONDERN auf der Höhe - und nur auf der- wo die beiden durch die Zweige zusammengeführt werden
# => je höher die zuammenführung desto weniger ähnlich
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
abline(h=3, col='red')

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

library(factoextra)
library(fpc)
library(NbClust)
fviz_dend(hc.average, show_labels = TRUE,
          palette = "jco", as.ggplot = TRUE)

## Bestimmen der Labels auf einem bestimmten Cut-Level
# Cut an Höhe
cutree(hclust.out, h = 7)
# Cut nach Anzahl von Clustern
cutree(hclust.out, k = 3)
xsc=scale(x)

## Skalieren der Variablen bevor Clustering vorgenommen wird
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")

## Korrelationsbasiertes Clustering
# (nur bei mind 3 Dimensionen sinnvoll)
# Korrelations-basierte Distanz durch as.dist() => dadurch für hclust verwendbar
x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")



######## Pokemon

#### Daten vorbereiten
pokemon <- read.csv('~/Documents/R/data/Pokemon.csv')
pokemon <- pokemon[,6:11]
names <- c('HitPoints', 'Attack', 'Defense', 'SpecialAttack', 'SpecialDefense', 'Speed')
colnames(pokemon) <- names
head(pokemon)

### Screeplot
wss <- 0
for (i in 1:15) {
  km.out <- kmeans(pokemon, centers = i, nstart = 20, iter.max = 50)
  wss[i] <- km.out$tot.withinss
}
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

### Modell bauen
k <- 3
km <- kmeans(pokemon, centers = k, nstart = 20, iter.max = 50)
km

## Defense vs. Speed nach cluster-zugehörigkeit
plot(pokemon[, c("Defense", "Speed")],
     col = km$cluster,
     main = paste("k-means clustering von pokemon mit", k, "clusters"),
     xlab = "Defense", ylab = "Speed")
