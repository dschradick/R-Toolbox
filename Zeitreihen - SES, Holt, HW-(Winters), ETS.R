########## TIME-SERIES - FORECASTING
# Buch: https://otexts.org/fpp2/
# vignette('JSS2008',package='forecast')
library(tidyverse)
library(readxl)
library(fpp2)
library(forecast)

#### Daten erzeugen
mydata <- read_excel("~/Documents/Data/exercise1.xlsx")
myts <- ts(mydata[, 2:4], start = c(1981, 1), frequency = 4)

#### Plots
## Faceting
autoplot(myts, facets = TRUE)
autoplot(myts, facets = FALSE)


## Drei Beispiel Serien
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)

## Outlier
goldoutlier <- which.max(gold)
goldoutlier

# Saisonale Frequenzen
frequency(gold)
frequency(woolyrnq)
frequency(gas)


## Saisonaler Plot
# polar = polar koord-system - Zeitachse ist zirkulär anstatt horizontal
autoplot(a10)
ggseasonplot(a10)
ggseasonplot(a10, polar = TRUE)

## (Saisonaler) Subseries Plot
# Mini time plots für jede Saison
# Blaue Linie = Mittelwert der Saison
frequency(beer)
autoplot(beer)
ggsubseriesplot(beer)

## Lagplot
gglagplot(beer)
gglagplot(beer,seasonal = F)

## ACF
ggAcf(beer)
acf(beer)

#### Ljung-Box Test
# Zur Überprüfung auf White-Noisen
# überprüft, ob die ersten h Autokorrelation als Gruppe
# sich so verhalten, wie man es von White-Noise erwartet
# Bsp: "Efficient Market Hypothesis" läßt sich damit belegen
# => falls H0 abgelehnt, ACF betrachten wo die Spikes sind
plot(beer)
Box.test(beer, lag=10, type="Lj")


#### Forecasting

### Naiv Forecasting
# benutz letzte Beobachtung
# => nicht besonders gut, aber sinnvoll als Benchmark
fcgoog <- naive(goog, h = 20)
autoplot(fcgoog)
summary(fcgoog)

## Naiv bei saisonalen Daten
# => Daten von letzer Saison benutzen
fcbeer <- snaive(ausbeer, h = 5 * frequency(ausbeer))
autoplot(fcbeer)

### Residuen überprüfen
goog %>% naive() %>% checkresiduals()       # white noise 
ausbeer %>% snaive() %>% checkresiduals()   # kein Whitenoise

### Forecast Accuarcy
# MAE & MSE: abhängig von Skala und kann nicht verschiedene Reihene vergleichen
# MAPE: nicht für Werte mit 0 oder sehr kleinen Werten - nimmt natürliche 0 an 
#       (z.B. Celcius geht nicht, weil 0 willkürlich gewählt)
# MASE: MAE / Q, wobei Q scaling konstante => kann für verschiedene serien verwendet werden
train <- subset(gold, end = 1000)
# Naiver Forecast & Mean Forecast
naive_fc <- naive(train, h = 108)
mean_fc <- meanf(train, h = 108)

# Forecast-Accuracy berechnen
accuracy(naive_fc, gold)
accuracy(mean_fc, gold)

#### Cross-Validation
# Econometrics: Forecast Evaluation on a rolling origin
# = letzte beochtung in jedem schritt (jeweils 1-schritt vorhersage)
# => kann auch auch der k-te schritt nach aktuellem trainsset sein => gemäß interesse

## Cross-Validation Fehler bis 8 Schritte weiter berechnen
e <- matrix(NA_real_, nrow = 1000, ncol = 8)
for (h in 1:8) {
  e[, h] <- tsCV(goog, forecastfunction = naive, h = h)
}

# MSE vs forcast horizontal plotten 
mse <- colMeans(e^2, na.rm = TRUE)
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()


#### Exponential Smoothing

### Single exponential smoothing
# \hat{Y}_{t+h|t} = \alpha y_t  + \alpha(1-\alpha)t_{t-1} + \alpha(1-\alpha)t_{t-2} + ... 
#
# Berechnet estimates für alpha und level_0 
# mit least square estimation (nicht geschlossen)
# Braucht nur Forecast horizon
## ses() für forecast von nächsten 10 jahren
fc <- ses(marathon, h = 10)
summary(fc)

# Plotting der Forecasts
autoplot(fc)
# One-Step Forecast für die Traings-daten
autoplot(fc) + autolayer(fitted(fc))


## SES vs naiv
train <- subset(marathon, end = length(marathon) - 20)
fcses <- ses(train, h = 20)
fcnaive <- naive(train, h = 20)
accuracy(fcses, marathon)
accuracy(fcnaive, marathon)
fcbest <- fcnaive


#### Holt's local trend method
# implementiert durch holt()
fcholt <- holt(austa, h = 10)
# Modell-Parameter anschauen
summary(fcholt)
autoplot(fcholt)
# Überprüfen ob Residuals white noise sind
checkresiduals(fcholt)

#### Damped vs Nicht Damped
set.seed(0)
a <- arima.sim(model = list(order = c(0, 2, 0)), n = 100)
autoplot(a)
fc1 <- holt(a, h = 50, PI = FALSE)
fc2 <- holt(a, damped = TRUE, h = 50, PI = FALSE)
autoplot(a) + xlab("Year") + ylab("millions") +
  autolayer(fc1, series="Linear trend") +
  autolayer(fc2, series="Damped trend")

#### Taxonomy
# N,N = Simple exponential smoothing: ses()
# A,N = Holt's linear method: holt()
# Alle anderen üer Holt-Winters Funktion: hw()

## Holt-Winters Method
x <- window(AirPassengers, start=1950)
autoplot(x)
# Forecast für 3 Jahre
fc1 <- hw(x, seasonal = "additive", h = 12)
fc2 <- hw(x, seasonal = "multiplicative", h = 12)
# Residuen überprüfen
checkresiduals(fc1)
checkresiduals(fc2) # besser
autoplot(fc1) 
autoplot(fc2)  # => bildet steigende Varianz ab


#### ETS Modelle
# Sind Modelle und keine Methoden
# => ets() liefert nicht Werte zurück, sondern Modell
# deshalb...
ets(austres) %>% forecast() %>% autoplot()

## Modell fitten
fitaus <- ets(austa)

## Diagnostik und und Plotten
checkresiduals(fitaus) # => ok
autoplot(forecast(fitaus))

## weiteres Beispiel
fiths <- ets(hyndsight)
checkresiduals(fiths)  # => nicht ok 
autoplot(forecast(fiths))


### Verlgeichen mit Cross-Validation
# Funktion die ETS Forecasts zurückgibt
fets <- function(y, h) {
  forecast(ets(y), h = h)
}

# Crossvalidierung von ets vs Seasonal Naive Forecasting
e1 <- tsCV(austa, fets, h = 4)
e2 <- tsCV(austa, snaive, h = 4)

# MSE berechnen 
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)

### Funktioniert nicht für alle Klassen
autoplot(lynx)
fit <- ets(lynx)
summary(fit)
fit %>% forecast(h = 20) %>% autoplot()


#### ARIMA

## Mit vs ohne drift
auto.arima(austa)
austa %>% Arima(order = c(0,1,1), include.constant = FALSE) %>% forecast() %>% autoplot()
austa %>% Arima(order = c(0,1,1), include.constant = TRUE) %>% forecast() %>% autoplot()
austa %>% Arima(order = c(2,1,3), include.constant = TRUE) %>% forecast() %>% autoplot()


#### ETS vs ARIMA 
# AIC zum Vergleich geht nicht, weil andere Modell-Klasse
# => mittels tsCV() vergleichen

### ETS vs ARIMA: Nicht-saisonalen Daten
fets <- function(x, h) {forecast(ets(x), h = h)}
farima <- function(x, h) {forecast(auto.arima(x), h = h)}

## CV-Errors berechnen
ets_e <- tsCV(austa, fets, h = 1)
arima_e <- tsCV(austa, farima, h = 1)

## MSE berechnen
mean(ets_e^2, na.rm = TRUE)     
mean(arima_e^2, na.rm = TRUE)
# => kleiner Fehler bei ARIMA
austa %>% fets(h = 10) %>% autoplot()
austa %>% farima(h = 10) %>% autoplot()

### ETS vs ARIMA: Saisonalen Daten
## 20 Jahre ab 1988
train <- window(qcement, start = 1988, end = c(2007, 4))
## Modell fitten
fit1 <- auto.arima(train)
fit2 <- ets(train)
checkresiduals(fit1)
checkresiduals(fit2)
fc1 <- forecast(fit1, h = 25)
fc2 <- forecast(fit2, h = 25)
# accuracy() für bestes Model basierend auf RMSE
accuracy(fc1, qcement)
accuracy(fc2, qcement) # => ETS besser





#### ARIMA auf seasonal data
# Check that the logged h02 data have stable variance
h02 %>% autoplot()             # Varianz steigt an
h02 %>% log() %>% autoplot()   # besser

# Mit boxcox lambda = 0 => logtransform
# Modell auf log-transformierte Daten gefittet
# aber forecasts werden auf Urspungsskala zurücktransformiert
fit <- auto.arima(h02, lambda = 0)
summary(fit)
fit %>% forecast(h = 24) %>% autoplot()

#### auto.arima mehr Modelle 
# stepwise = FALSE  => probiert mehr Modelle

fit1 <- auto.arima(euretail)
fit2 <- auto.arima(euretail, stepwise = FALSE)
fit1$aic # 69.24317
fit2$aic # 67.25823 => besser
fit2 %>% forecast(h = 8) %>% autoplot()



