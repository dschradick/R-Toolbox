library(xts)
library(TTR)
library(corrplot)
library(PerformanceAnalytics)
library(astsa)
# Wenn xts Object, dann plot() = plots.xts()


#### Daten
eu_stocks <- EuStockMarkets
ts <- eu_stocks[,1]
ts1 <- eu_stocks[,1]
ts2 <- eu_stocks[,2]
multi <- eu_stocks

#### Einfaches Plotting
plot(ts, main = "Titel", xlab = "Datum", ylab = "Preis")   # Beschriftungen
plot(ts, lwd = 3, col = 'red')                             # Farbe + Dicke
plot(ts, type='h')                                         # Bars

#### Übereinander
par(mfrow = c(2,1), mex = 0.6, cex = 0.8)
plot(ts)
plot(ts2)

#### Zwei Skalen
par(mfrow = c(1,1))
plot(ts1)
lines(ts2, col = "red")
axis(side = 4, at = pretty(ts2))
legend(x = "bottomright", 
       legend = c("1", "2"), 
       col = c("black", "red"),
       lty = c(1, 1))


#### Highlighting
plot(ts)
vert_line <- which(index(ts) == as.Date("1995-01-01"))
abline(v = .index(ts)[vert_line], col = "red")
hori_line <- mean(ts1)
abline(h = hori_line, col = "blue")


# 2x2 Univariat Diagnostik
par(mfrow = c(2, 2))
hist(return, probability = TRUE)
lines(density(return), col = "red")
boxplot(return)
acf(return)
qqnorm(return)
qqline(return, col = "red")

#### MULTIVARIAT 
# Interaktionen zwischen TS
# z.B. wie reagiert einzelner Kurs auf interest rate change
#      wie reagieren mehrere Kurse auf interest rate change
# => Muster erkennen

barplot(multi)
barplot(multi, beside = TRUE)

## Bivarait - Scatterplot
# Draw the scatterplot
par(mfrow = c(1, 1))
plot(ts1, ts2)
abline(reg = lm(ts2 ~ ts1), col = "red", lwd = 2)

### Mehrere Plots in einem
# Mehrere Unterplots
plot.zoo(edhec, plot.type = "multiple")
# Alles in einen Plot
plot.zoo(edhec, plot.type = "single")

### Korrelation
# Textuell
cor(multi)
cor(multi, method = "spearman")
# Grafisch
cor_mat <- cor(multi)
corrplot(cor_mat)
corrplot(cor_mat, method = "number")
corrplot(cor_mat, method = "color")
corrplot(cor_mat, method = "number", type = "upper")
corrplot(cor_mat, method = "color", type = "upper", order = "hclust")

## Scattermatrix
pairs(my_data)
pairs(my_data, lower.panel = NULL, main = "Stocks Correlation Scatterplots")

