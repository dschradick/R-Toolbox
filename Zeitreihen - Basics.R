############ TIME SERIES - BASICS
# http://r-statistics.co/Time-Series-Analysis-With-R.html
#install.packages('fpp2')
library(datasets)
library(PerformanceAnalytics)
library(forecast)
library(fpp2)
eu_stocks <- EuStockMarkets


#### Basics
print(Nile)
## Anzahl der Einträge
length(Nile)
## Head und Tail von Timeseries
head(Nile, n = 10)
tail(Nile, n = 12)

## Plotting mit Base
plot(Nile)
lines(Nile, col='red',lwd=2, type = "h") # col = farbe, lwd=dicke
par(mfrow=c(1,1))
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})", main = "Annual River Nile Volume at Aswan, 1871-1970", type = "b")

## Reduzierte Margins
# Größe von margins und characters im Text => mex, cex
par(mfrow = c(2,1)); plot(Nile,main="Title"); plot(Nile,main="Title") # vs...
par(mfrow = c(2,1), mex = 0.6, cex = 0.8); plot(Nile,main="Title"); plot(Nile,main="Title")
par(mfrow = c(1,1))

## Erweitere Plots mit ts.plot()
ts.plot(EuStockMarkets)
ts.plot(EuStockMarkets, col = 1:4, xlab = "Year", ylab = "Index Value", main = "Major European Stock Indices, 1991-1998")
legend("topleft", colnames(EuStockMarkets), lty = 1, col = 1:4, bty = "n")

## Vollständiger Plot mit base package
head(EuStockMarkets)
plot(EuStockMarkets[,1], main="Title")
lines(EuStockMarkets[,2], col = "red")
# Legende hinzfügen
legend(x = "bottomright", 
       legend = c("DAX", "SMI"),  # Beschriftungen zuordnen
       col = c("black", "red"),   # Farben zuordnen
       lty = c(1, 1))             # Linetypes zuordnen 
# Rechte Axe hinzufügen
axis(side = 4, at = pretty(EuStockMarkets[,2]))
# Vertikale und horizontale Line hinzufügen
abline(v = 1995.5, col = "red")
abline(h = 3500, col = "blue")
# Zeitabschnitt highlighten
# braucht xts
period <- c("1995-01-01/1996-01-01")
chart.TimeSeries(EuStockMarkets[,2], period.areas = period)
chart.TimeSeries(EuStockMarkets[,2], period.areas = period, period.color = "lightgrey")                   

#### Dekomposition
# STL: “Seasonal and Trend decomposition using Loess”
# t.window = trend-cycle window 
# s.window = seasonal window
# c(), trendcycle(), remainder() für einzelne Komponenten
# seasadj() für seasonally adjusted series
EuStockMarkets[,1] %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()
## mstl - wählt windows automatisch
EuStockMarkets[,1] %>%
  mstl() %>%
  autoplot()
AirPassengers %>%
  mstl() %>%
  autoplot()

#### Sampling Frequenz
par(mfrow=c(1,1))
plot(AirPassengers)
str(AirPassengers)

## Start- und End-Datum
start(AirPassengers) # start(): time index von erster Beobachtung 
end(AirPassengers)   # end():   time index von letzter Beboachtung

## Typische Funktionen
# time():       Vektor von Zeit-Indizes mit einem Element für jeden Zeit Index in dem die Series beobachtet wurde
# deltat():     festes Zeitinterval zwischen Beobachtungen
# frequency():  Anzahl der Beobachtungen pro Zeit-Einheit 
# cycle():      Postion im Zyklus für jede Beobachtungfunction returns the position in the cycle of each observation.
time(AirPassengers)
deltat(AirPassengers)
frequency(AirPassengers)
cycle(AirPassengers)


#### Time Series Object erstellen
# mit ts()
# Überprufen: is.ts()
data_vector <- as.numeric(AirPassengers)
plot(data_vector)
# Erzeugen
time_series <- ts(data_vector, start = 1949, frequency = 12)
is.ts(time_series)
plot(time_series)



#### Entfernen von Trends...

## ...In Level durch differencing
# => kann den time-trend in series entfernen
# => erlaubt es die incremente / Veränderungen zu untersuchen
# z[t] besteht aus den Differenzen zwischen sukzessiven Beobachtungen über die Zeit,
# so dass z[t] = z[t] − z[t−1]
z <- arima.sim(model = list(order = c(0, 1, 0)), n = 200, mean = 1)
ts.plot(z)  
dz <- diff(z)
ts.plot(dz)  
length(z) ; length(dz) # => eine Beobachtung weniger


## ...von seasonal trends mit seasonal differencing
# Series hat seasonal trend => seasonal differencing kann diesess periodsichen Muster entfernen
# Bsp: monatliche Daten können 12-Monats Muster aufweisen
#    => wenn Veränderung im Verhalten von Jahr zu Jahr von größerem Interesse
#       als Monat zu Monat, welches ein seasonal pattern aufweisen könnte
#
# diff(..., lag = s) berechnet den lag s difference oder length s seasonal change
# => monatliche, vierteljährige Daten => s=12 / s=4
# => s=1 ist normales differencing
x <- AirPassengers
dx <- diff(x, lag = 12)
ts.plot(dx)  
length(x) ; length(dx)

## ...in Variabilität durch log-transform
# => wenn serie Varianz aufweisst die über die Zeit größer wird, wird diese stabilisiert
plot(AirPassengers)
log_growth <- log(AirPassengers)
ts.plot(log_growth)  

### Stationär
# Stationäre Prozesse haben verteilungs-stabilität über die Zeit
# => Warum: können mit weniger Parametern modelliert werden 
#           (weil z.B. benötigt keine verschiedenen Erwartungen für Y_t sondern \mu = sample-mean)
# Eigenschaften:
# Weak Stationary: Mittelwert, Varianz, Kovarianz ist konstant über die Zeit
# => verändern sich nicht durch time-shifts - Cov(Y_2,Y5) = Cov(Y_7,Y10) (beide drei ausseinander)
# => Flukturation ist zufällig aber verhalten sich 
#    ähnlich von einer Zeitperiode zur nächsten
#
# Problem: viele Zeitreihen nicht stationär 
# => dafür aber die changes in der Serie (z.B. nach log-transformation)
# - stationäre oszilieren zufällig um einen bestimmten 
#   => Phänomen heisst: mean-reversion
#      Bsp: Inflation an sich nicht, aber die (log?) changes davon oszilieren um 0


## Random Walks sind nicht stationary
# egal ob mit oder ohne drift
white_noise <- arima.sim(model = list(order = c(0, 0, 0)), n = 100)
# Konvertieren von WN nach RW
random_walk <- cumsum(white_noise)
# Generien von WN mit drift 
wn_drift    <- arima.sim(model = list(order = c(0, 0, 0)), n = 100, mean = 0.4)
# Konvertieren WN drift Daten nach RW
rw_drift    <- cumsum(wn_drift)
plot.ts(cbind(white_noise, random_walk, wn_drift, rw_drift))


#### Autokorrelation
# Autokorrelations an mehreren lags schätzen um besser zu bestimmen,
# wie sie sich zu ihrer Vergangenheit verhält.
# => meist hauptsächlich an der letzter Zeit interessiert
# acf(..., lag.max = ..., plot = FALSE)  
# schätzt alle Autokorrelationen 0,1,... bis lag.max 
# Schätzen der der ACF von x mit lags 0 bis 10
acf(AirPassengers, lag.max = 10, plot = FALSE)


## Visualisieren der Autokorrelation(s-Funktion) als Funktion des lag
# blaue linien = lag-wise 95% CI zentriert um 0
# Werte bei dem die Autokorrelationen statistisch signifikant verschieden von 0
# => für statistische signifikanz von individueller autokorrelation schätzung 
#    bei gegeben lag vs null wert von 0 - also kein Autokorrelation
par(mfrow=c(1,1))
rw <- arima.sim(model = list(order = c(0, 0, 0)), n = 200)
ts.plot(AirPassengers)   # starke persistenz: aktueller wert sehr nahe am vorigen
ts.plot(rw)              # kein pattern
acf(AirPassengers)       # hohe korrelation 
acf(rw)                  # nicht linear verwandt mit seiner vergangenheit


#### Autoregression
#  Simulieren des Autoregressiven Models
# list(ar = phi), wobei phi= slope parameter vom Interval (-1, 1). 
x <- arima.sim(model = list(ar = 0.5), n = 100);  acf(x)    # moderat
y <- arima.sim(model = list(ar = 0.9), n = 100);  acf(y)    # starke autokorrelation
z <- arima.sim(model = list(ar = -0.75), n = 100);acf(z)    # wenig Autokorrelation
plot.ts(cbind(x, y, z))


## Verteilungen der Zeitreihen betrachten
# Hier: Die meistens returns fast 0, aber 
#        einige wenige Tage an denen es Ausreisser gibt
colMeans(returns) 
apply(returns, MARGIN = 2, FUN = var)
apply(returns, MARGIN = 2, FUN = sd)
apply(returns, MARGIN = 2, FUN = hist, main = "", xlab = "Percentage Return")
apply(returns, MARGIN = 2, FUN = qqnorm, main = "")
qqline(returns)


### Asset return & Log return
# Index Werte entsprechen Preis
## Konvertieren von Preis nach Return
plot(eu_stocks)
returns <- eu_stocks[-1,] / eu_stocks[-1860,] - 1
returns <- ts(returns, start = c(1991, 130), frequency = 260)
plot(returns)
## Konvertieren des Preis nach log-returns
logreturns <- diff(log(eu_stocks))
plot(logreturns)


### Korrelation & Korrelation zwischen Zeitreihen
# Bivariate Beziehung zwischen Zeitreihen betrachten
# => auch als log-return
DAX <- eu_stocks[,1]; FTSE <- eu_stocks[,4]
DAX_logreturns <- diff(log(DAX)); FTSE_logreturns <-diff(log(FTSE))
## Visualisieren
# Scatterplot
plot(DAX, FTSE)
# Scatter-Matrix
pairs(eu_stocks)
# Auch für log-returns
logreturns <- diff(log(eu_stocks))
plot(logreturns)
pairs(logreturns)


## Berechnen
# Kovarianz
cov(DAX_logreturns, FTSE_logreturns)
# Kovarianz-Matrix
cov(logreturns)
# Korrelation
cor(DAX_logreturns, FTSE_logreturns)
# Korrelations-Matrix
cor(logreturns)


### Autokorrelation berechnen
x <- DAX; n <- length(DAX)
x_t0 <- x[-1] 
x_t1 <- x[-n]

## Lag-1 Autokorrelation
# zuerst: manuelle lag-1 Autokorrelation
# Korrelation zwischen x[t], x[t-1] 
# = Korrelation von "heute" mit "gestern"
# => eine Zeiteinheit auseinander 
head(cbind(x_t0, x_t1))
plot(x_t0, x_t1)
cor(x_t0, x_t1)

# ACF Funktion 
# = shortcut für Berechnung der Autokorrelation
# => verschiedener Wert zu manuell, wg anderem scaling
#    bei Berechnung der sample Kovarianz:  1/(n-1) versus 1/n
acf(x, lag.max = 1, plot = F)
cor(x_t1, x_t0) * (n-1)/n   # ...dann gleich
## Textuell vs grafisch
acf(x, lag.max = 10, plot = F)
acf(x, lag.max = 10, plot = T)








