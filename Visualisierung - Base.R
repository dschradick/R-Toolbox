########## PLOTTING BASE
library(sm)
library(MASS)
attach(mtcars)

## Multiplot
par(mfrow=c(1,1)) # Reihen,Spalten

## Scatterplot & Regressionsgerade
plot(wt, mpg) # = plot(mpg ~ wt) 
abline(lm(mpg ~ wt))
title("Regression MPG vs Weight")

## Histogram & Density
hist(mtcars$mpg, breaks=12, col="red")
truehist(mtcars$mpg)        # skaliert count auf schätzung der dichte (aus MASS)
lines(density(mtcars$mpg))  # density hinzufügen

## Dichtefunktion
d <- density(mtcars$mpg) 
plot(d) 

## Scatterplot (vs index) Univariat
plot(mtcars$mpg) # (gut für Outlier)

## Scatterplot
plot(mtcars$mpg ~ mtcars$qsec)

## ab-line - Intercept, Slope
abline(0,1)

## Boxplot
boxplot(mpg~cyl,data=mtcars, main="Zylinder vs MPG",  xlab="Anzahl von Zylindern", ylab="MPG")
# Log Transform im Boxplot 
boxplot(crim ~ rad, data = Boston, varwidth = TRUE, las = 1)             # vs... 
boxplot(crim ~ rad, data = Boston, varwidth = TRUE, log = "y", las = 1)

## Mosaic-Plot
# => für zwei kategorische Variablen
mosaicplot(carb ~ cyl, data = mtcars)

## Bagplot
# zeigt Scatterplot mit median, min, max und lower/upper quantile
# => gut
library(aplpack)
bagplot(Boston$RM, Boston$medv, cex = 1.2)

## Barplot
counts <- table(mtcars$gear)
barplot(counts, main="Verteilung",xlab="Gears")

## Side-by-Side Barplot
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Gears und VS",
        xlab="Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

## Scattermatrix
pairs(~mpg+disp+drat+wt,data=mtcars, 
      main="Scatterplot Matrix")


## Pie-Chart
slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls, main="Pie Chart von Ländern")



## 2D Dot-Plot
dotchart(mtcars$mpg,labels=row.names(mtcars),cex=.7,
         main="MPG für Modelle", 
         xlab="MPG")

## Gruppierter Dotplot
# Sortiert nach mpg und gruppiert durch color und cylinder 
x <- mtcars[order(mtcars$mpg),] 
x$cyl <- factor(x$cyl) 
x$color[x$cyl==4] <- "red"; x$color[x$cyl==6] <- "blue"; x$color[x$cyl==8] <- "darkgreen"	
dotchart(x$mpg,labels=row.names(x),cex=.7,groups= x$cyl,
         main="",
         xlab="MPG", gcolor="black", color=x$color)

## Linien
# p	Punkte
# l	Linie
# o	überlagert Punkte und Linie
# b, c	Punkte (c=leer) durch Linien verbunden
# s, S	Treppenstufen
# h	histogram-like vertikale Linien

x <- c(1:5); y <- x 
par(pch=22, col="red") 
par(mfrow=c(2,4)) 
opts = c("p","l","o","b","c","s","S","h") 
for(i in 1:length(opts)){ 
  heading = paste("type=",opts[i]) 
  plot(x, y, type="n", main=heading) 
  lines(x, y, type=opts[i]) 
}

## Sunflower Plot
# für sich wiederholende numerische Werte
par(mfrow = c(1, 2))
plot(Boston$zn, Boston$rad)
sunflowerplot(Boston$zn, Boston$rad)
