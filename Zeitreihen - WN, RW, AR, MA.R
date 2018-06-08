########## TIME-SERIES - BASIS-MODELLE WN, RW, AR, MA

#### White Noise
# Ist der einfachste stationäre Prozess
# Eigenschaften:
# - konstanter Mittelwert
# - konstante Varianz
# - keine Korrelation über die Zeit
# ===> muss alles zutreffen!!
#
# White-Noise = ARIMA(0,0,0)
# Simulation durch arima.sim()
## WN Modell mit list(order = c(0, 0, 0))
white_noise<- arima.sim(model = list(order = c(0, 0, 0)), n = 100)
ts.plot(white_noise)

## Simulieren von WN Modell mit mean = 100, sd = 10
white_noise_2 <- arima.sim(list(order = c(0, 0, 0)), n = 100, mean = 100, sd = 10)
ts.plot(white_noise_2)

## Schätzen des WN-Modells
arima(white_noise_2,order = c(0, 0, 0))
# => wie erwartet: mean = ~100, und sigma^2 = 100 = sd^2 = 10^2

# Direkt schätzen
mean(white_noise_2)
var(white_noise_2)



#### Random Walk
# Einfaches Beispiel eines nicht-stationären Prozesses.
# Today = Yesterday + Noise,  Y_t = Y_{t-1} + \epsilon_t, wobei fehler WN ist
# Mit Drift: Today = Constant + Yesterday + Noise,  Y_t = c + Y_{t-1} + \epsilon_t, wobei fehler WN ist
# => nur ein Parameter: Varianz des WN (bzw 2 bei drift für c)
# Eigenschaften: 
# - kein spezifizierter Mittelwert oder Varianz
# - starke abhängig über die Zeit
# - veränderung / inkrements sind WM - folgen white-noise Prozess (welcher stable & stationary)
# => Difference ist White Noise (mit mean = c)
# Ist kumulativer Summe (=Intergration)  einer mean zero white noise Reihe
# so dass die first difference Reihe eines RW ene WN Serie ist

## Generien eine Random Walks mit arima.sim()
# => RW model ist ARIMA(0, 1, 0) Modell, in dem mittlerer Term = 1
#    also models order of integration ist gleich 1
random_walk <- arima.sim(model = list(order = c(0, 1, 0)), n = 100)
ts.plot(random_walk)
# First difference berechnen
random_walk_diff <- diff(random_walk)
ts.plot(random_walk_diff) 
# => White Noise Daten



## Random Walk mit Drift
# Muss nicht um 0 herumwandern, sondern RW kann trend haben
# mean = 1 für die Slope
rw_drift <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean =0.3)
ts.plot(rw_drift)
# First difference berechnen
rw_drift_diff <- diff(rw_drift)
ts.plot(rw_drift_diff) 
# => wieder Whitenoise-Modell - unabhängig vom Drift


## Schätzen des White-Noise Modells
# Zu gegebener Zeihreihe Y kann man RW Model mit drift fitten,
# duch first differencing der Daten und dann ein White-Noise Modell
# der differenced Daten mit arima mit order = c(0, 0, 0)) erstellt
#
ts.plot(random_walk)
# Difference des RW
rw_diff <- diff(random_walk)
ts.plot(rw_diff)
# Fitten eines WM Modells auf differenced Daten
model_wn <- arima(rw_diff, order = c(0, 0, 0))

# Geschätzer Time-trend (Intercept)
int_wn <- model_wn$coef
# Plot the original random_walk data
ts.plot(random_walk)
# Time-Trend hinzufügen
abline(0, int_wn)




#### Autoregressives Modell
# Einfachste Form: 1. Ordnunung - heutige Beobachtung wird auf gestrige regressed
# Today = Constant + Slope * Yesterday + Noise
# Mean Centered: (Today-Mean) = Slope * (Yesterday - Mean) + Noise
# Y_t - μ = \phi(Y_{t-1} * μ) + \epsilon_t, wobei \epsilon_t mean-zero WN
# => Slope phi = 0 => Y ist WN-Prozess
# => Slope phi <> 0 => Y ist autokorreliert und hängt von \epsion_t und Y_{t_1} ab
# => größere Werte von phi => stärkere Autokorrelation
# => negative Wete von phi => ts oszilliert
# Interpretation wie bei linearer Regression, aber regression auf vorigen Beobachtungen.
# AR-Modell beinhaltet das WN und RW Modell als spezielle Fälle.

# arima.sim() zum simluieren von Daten eines AR durch list(ar=phi), 
# wobei phi slope-Paramete [-1,1]

## Simulieren von Daten eines AR-modell mit... 
x <- arima.sim(model = list(ar = 0.5), n = 100)     # Slope =  0.5  = wenig Autokorrelation
y <- arima.sim(model = list(ar = 0.9), n = 100)     # Slope =  0.9  = starke Autokorrelation
z <- arima.sim(model = list(ar = -0.75), n = 100)   # Slope = -0.75 = oszililiert von Wert zu Wert
plot.ts(cbind(x, y, z))
# Schätzen der zugehörigen Autokorrelations-Funktion
acf(x)
acf(y)
acf(z)

### AR versus Random Walk
# AR mit slope = 0.9 
x <- arima.sim(model = list(ar = 0.9), n = 200)
ts.plot(x)
acf(x) 
# AR mit slope = 0.98  
y <- arima.sim(model = list(ar = 0.98), n = 200)
ts.plot(y)
acf(y)  # => höhere persistence, aber geht zu 0
# RW Model 
z <- arima.sim(model = list(order=c(0,1,0)), n = 200)
ts.plot(z)  
acf(z)  # hohe persistence und nur wenig sinken im acf
AR <- arima(AirPassengers, order = c(1, 0, 0))
print(AR)


### Schätzen eines AR-Modells
AR <- arima(AirPassengers, order = c(1, 0, 0))
print(AR)
# => Estimated Slope - ar1           : 0.9646
#    Mean - Intercept:               : 278.4649
#    Innovation Variance (sigma^2)   : 1.022
ts.plot(AirPassengers)
AR_fitted <- AirPassengers - residuals(AR)
points(AR_fitted, type = "l", col = 2, lty = 2)


### Forecasting von geschätztem AR-Modell
## Model fitten
AR_fit <- arima(Nile, order = c(1,0,0))
print(AR_fit)

## 1-step forecast durchführen
predict_AR <- predict(AR_fit)
predict_AR$pred[1]

## 10 Schritte vorhersagen
predict_AR <- predict(AR_fit, n.ahead = 10)
predict_AR$pred

## Forecast und 95% prediction intervals
ts.plot(Nile, xlim = c(1871, 1980))
AR_forecast <- predict(AR_fit, n.ahead = 10)$pred
AR_forecast_se <- predict(AR_fit, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
# => breites konfidenz-band, aufgrund geringer Persistence des Zeitreihe




#### Simple Moving Average
# Model für sehr kurz-laufende Autokorrelationen
# Hat auch Form einer Regression, aber die einzelnen Beobachtungen werden 
# auf der vorigen Innovation regressiert, welche nicht tatsächlich beobachtet wird.
# Wie das AR-Modell beinhaltet das MA-Modell ebenfalls das WM und RW Modell als Spezialfälle.
#
# MA Model simuliert durch arima.sim() mit list(ma = theta), 
# wobei theta slope Parameter aus (-1,1)

## Erzeugen von MA-Modells mit...
x <- arima.sim(model = list(ma = 0.5), n = 100)   # Slope =  .5  => short-run persistence
y <- arima.sim(model = list(ma = 0.9), n = 100)   # Slope =  .9  => short-run persistence
z <- arima.sim(model = list(ma = -0.5), n = 100)  # Slope = -.5  => alterniert schneller
plot.ts(cbind(x, y, z))

## zugehörige ACFs schätzen
acf(x)
acf(y)
acf(z)


## Schätzen eines MA-Modells
# =  ARIMA(0, 0, 1) 
MA <- arima(Nile, order = c(0, 0, 1))
print(MA)
ts.plot(Nile)
MA_fit <- Nile - resid(MA)
points(MA_fit, type = "l", col = 2, lty = 2)


## Forecasting mit MA-Modell
# MA-Modell kann nur ein 1-step forecast machen (!) - 
# für zusätzliche forecasts erweitert predict() 
# einfach die originale 1-step Vorhersage
# 1-step forecast
predict_MA <- predict(MA)
predict_MA$pred[1] 
# Forecast mit mehreren Schritten
predict(MA, n.ahead = 10)
# Plot von Nile Reihe mit 95% prediction interval
ts.plot(Nile, xlim = c(1871, 1980))
MA_forecasts <- predict(MA, n.ahead = 10)$pred
MA_forecast_se <- predict(MA, n.ahead = 10)$se
points(MA_forecasts, type = "l", col = 2)
points(MA_forecasts - 2*MA_forecast_se, type = "l", col = 2, lty = 2)
points(MA_forecasts + 2*MA_forecast_se, type = "l", col = 2, lty = 2)


#### Evaluieren
# Mit AIC oder BIC 
# - bestrafen Modelle mit mehr Parametern, um Overfitten zu reduzieren
# - kleiner ist besser

## Korrelation zwischen den Prediction
cor(AR_forecast, MA_forecasts) # Vorhersagen für die 1970er
# => hohe Korrelation

## Mit AIC vergleichen
AIC(AR) ; AIC(MA)  
## Mit BIC vergleichen
BIC(AR) ; BIC(MA)
# => AR Modell besser - leicht geringerer Wert 

