########## TIME-SERIES - ARIMA & SARIMA
library(xts)
library(astsa) 
library(forecast)
library(CombMSC)
library(tidyverse)
library(tseries)
# Kein Plotting über border
par(xpd=FALSE)  

#### Detrending 

## Detrending von trend stationary reihe & random walk
# => differencing
x <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean =0.3)
autoplot(x)
autoplot(diff(x))

## Augmented Dickey Fuller 
adf.test(x)        # nicht-stationär
adf.test(diff(x))  # stationär

## Detrending trend stationary + heteroscedasticity
# => log + differencing
par(mfrow = c(2,1)) ; plot(gnp) ; plot(diff(log(gnp)))
par(mfrow = c(2,1)); plot(djia$Close)
plot(diff(log((djia$Close))))
par(mfrow = c(1,1))


#### ARMA
# arima.sim(model, n, ...)
# model = liste mit ordnung des Modelells als c(p, d, q) 
#         und den koeffizienten
# p = Ordnung von AR
# q = Ordnung von MA
# n = Länge der Reihe
#
# MA(1): X_t = W_t + 0.9 * W_{t-1}
x <- arima.sim(list(order = c(0,0,1), ma = 0.9), n = 100); autoplot(x)
# AR(2): X_t = -0.9 * X{t_2} + W_{t}
x <- arima.sim(list(order = c(2,0,0), ar = c(0,0.9)), n = 100); autoplot(x)


### Verschiedene Modelle
## Generien von White Noise Model
WN <- arima.sim(model = list(order = c(0, 0, 0)), n = 200); autoplot(WN)
## MA(1) mit parameter .9 durch filtern des noise
MA <- arima.sim(model = list(order = c(0, 0, 1), ma = .9), n = 200); autoplot(MA)
# AR(1) with parameters 1.5 and -.75
AR <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -.75)), n = 200); autoplot(AR)


#### Identifikation des Modells
# Mittels ACF und PACF
#             AR(p)                   MA(q)               ARMA(p, q)
# ACF       Tails off          Cuts off nach lag q       Tails Off
# PACF   Cuts off nach lag p        Tails off            Tails off
ar1 <- arima.sim(model = list(order = c(1, 0, 0), ar = -.7), n = 200) 
ma1 <- arima.sim(model = list(order = c(0, 0, 1), ma = -.7), n = 200) 
autoplot(ar1, main="AR(1)")
autoplot(ma1, main="MA(1)")
acf(ar1)    # Tails off
pacf(ar1)   # Cuts of after lag 1
acf(ma1)    # Cuts of after lag 1
pacf(ma1)   # Tails off

### ACF + P/ACF = acf2
# ACF + P/ACF zusammen + Korrelationswerte
acf2(x)


### sarima
# wie arima()
# => mehr Details & erzeugt Diagnostics 
## AR1 Model
sarima(x, p = 1, d = 0, q = 0)



#### Modell aus Formel erzeugen

## AR(2)
# X_t = 1.5*X_{t−1} − .75*X{t−2} + W_t
x <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -.75)), n = 200)
autoplot(x)
ggAcf(x)
sarima(x, p = 2, d = 0, q = 0) # ar1 = 1.5; ar2 = -0.79

## MA(1)
# X_t = W_t − .8*W_{t−1}
x <- arima.sim(model = list(order = c(0, 0, 1), ma = c(-.8)), n = 100)
autoplot(x)
acf2(x)
sarima(x, p = 0, d = 0, q = 1)  # ma1 = -0.81

## ARMA(2,1)
# X_t = X_{t−1} −.9*X{t−2} + W_t + .8*W_{t−1}
x <- arima.sim(model = list(order = c(2, 0, 1), ar = c(1, -.9), ma = .8), n = 250)
plot(x)
acf2(x)
sarima(x, p = 2, d = 0, q = 1)


#### Model bestimmen
dl_varve <- diff(log(varve))
acf2(dl_varve)                        # => sieht nach MA(1) aus
## MA(1)
sarima(dl_varve, p = 0, d = 0, q = 1) # AIC: -0.4374168
# besteht noch Autokorrelation in Residuen gemäß ACF und Ljung-Box statistik 
# => MA(1) Residuen sind nicht white noise
## MA(2)
sarima(dl_varve, p = 0, d = 0, q = 2) # AIC: -0.4629629
## ARMA(1,1)
sarima(dl_varve, p = 1, d = 0, q = 1) # AIC: -0.467376
# => Residuen sind gaussian white noise

## Zweites Beispiel
plot(oil)                      # nicht stationär
oil_returns <- diff(oil)       # => differenzieren
plot(oil_returns)              # immernoch keine konstante varianz
oil_returns <- diff(log(oil))  # log transformation + diff
plot(oil_returns)
acf2(oil_returns)              # beide tailing off => ARMA
sarima(oil_returns, p = 1, d = 0, q = 1)
# => hat noch ausreisser in den residuals


#### ARIMA
# Integrated ARMA
# Zeitreihe weisst ARIMA Verhalten auf, wenn differenzierte Daten ARMA Verhalten haben
# d = 1 => erhält stationäre Reihe, wenn man einmal differenziert

## ARIMA vs ARMA = differenced ARIMA
x <- arima.sim(list(order = c(1, 1, 0),  ar = .9), n = 200)
plot(x, main = "ARIMA(p = 1, d = 1, q = 0)")         # ARIMA Daten
plot(diff(x), main = "ARMA(p = 1, d = 0, q = 0)")    # ARMA Daten

### ACF und PCF von differenced ARIMA
x <- arima.sim(list(order = c(1, 1, 0),  ar = .9), n = 200)
acf2(x)       # ARIMA
acf2(diff(x)) # differenced ARIMA = AR(1)

### Arima aus Formel
# ARIMA(2,1,0) mit
# Y_t= 1 + 1.5*Y_{t−1} −.75*Y_{t−2} + W_t
# wobei Y_t = ∇X_t = X_t − X_{t−1}
x <- arima.sim(list(order = c(2, 1, 0),  ar = c(1.5,-.75)), n = 200)
acf2(diff(x))
sarima(x, p = 2, d = 1, q = 0) # AR(2) mit nahezu richtigen Parametern


## Globaltemp daten 
plot(globtemp)         # random walk=> differencing
plot(diff(globtemp))   # stationär
acf2(diff(globtemp))   
# Entweder...
# - beide tailing off => ARIMA(1,1,1)
# - ACF cut-off bei 2 PACF tailing => ARIMA(0,1,2)
# - ACF tailing off und PACF cuts off bei lag 3 => ARIMA(3,1,0)
#    => Aber zuviele Parameter für so kleine Autokorrelation
sarima(globtemp, p = 1, d = 1, q = 1) # => bestes
sarima(globtemp, p = 0, d = 1, q = 2)
sarima(globtemp, p = 3, d = 1, q = 0)
sarima.for(globtemp, n.ahead = 35, p = 1, d = 1, q = 1) 

## Parameter hinzu & signifikanz testen
sarima(oil, p = 1, d = 1, q = 1) # alle signifikant
sarima(oil, p = 1, d = 1, q = 1) # MA2 nicht signifikat
sarima(oil, p = 2, d = 1, q = 1) # AR2 nicht signifkant
# => bei mehreren mit AIC & BIC wählen
# => ARIMA(p = 1, d = 1, q = 1) bestes model

## Forecasting
oilt  <- window(astsa::oil, end = 2006)
oilf <- window(astsa::oil, end = 2007)
sarima.for(oilt, n.ahead = 52, 1, 1, 1) # rote linie = forecast, zweites grau  = 2 mean square prediction error  = 95% prediction interval
lines(oilf) # schwarze linie = echte daten


#### Seasonal ARIMA
# z.B. Airpassenger 1 cycle jede S = 12 monate
## Pure Seasonal Models
# = Nur abhängig von seasonal component => selten
# Bsp: SAR(P = 1)_12


## Mixel Seasonal Models
# gemischte abhängigkeit 
# => nur ein Teil der Varianz kann durch seasonal trend erklärt werden
# SARIMA(p,d,q) x (P,D,Q)_S  => Grossbuchstaben für seasonal order von AR,...
# Bsp: SARIMA(0, 0, 1) x (1, 0, 0)_12
#      X_t = \Phi{t-12} + W_t + \thetaW_{t-1}
# Wetter diesen Mai hängt vom Wetter im April und vom Wetter letztes Jahr Mai ab
# also von X_{t-12} und shock / error von W_{t-1}


### Modeling

## Time-Series stationär machen
x    <- AirPassengers       # Varianz steigt => log
xd   <- log(x)              # Trend => differencing
xdl  <- diff(xd)            # Seasonal persistence mit 3 cycles pro jahr
xdls <- diff(xdl,12)
plot.ts(cbind(x, xd, xdl, xdls))

## Modell bestimmen
acf2(xdls)
# Seasonal: ACF cut off bei lag 1s (s = 12); PACF tails off bei lags 1s, 2s, 3s...
# Non-Seasonal: ACF and PACF both tailing off
# => SARIMA(1,1,0) x (1,1,1)_12
airpass_fit1 <- sarima(log(AirPassengers), p = 1, d = 1, q = 1, 
                       P = 0, D = 1, Q = 1, 
                       S = 12)

## Modell optimieren
airpass_fit1$ttable # AR1 nicht signifikant - rausnehmen
airpass_fit2 <- sarima(log(AirPassengers), 0, 1, 1, 0, 1, 1, 12)
airpass_fit2$ttable



### Beispiel Unemployment
plot(unemp)                            # Hat Trend
d_unemp <- diff(unemp)                 # Differenzieren
plot(d_unemp)                          # noch seasonal persistence!!!
dd_unemp <- diff(d_unemp, lag = 12)    # seasonal differencing
plot(dd_unemp)                         # nun stationär
acf2(dd_unemp, max.lag = 60)           # sample ACF und PACF für 5 jahre
# !! sample acf/pacf lags 1, 2, 3, ... repräsentieren Jahre - 1 jahr (12 monate), 2 jahre (24 monate), ...
# !! Seasonal Component: Nach Interpreationstablle bei 1,2,3... vergleichen
# !! Non-Seasonal Component: Nach Interpreationstablle einzelne Werte im Bereich von 0-1 betrachten
# Nonseaonal component: PACF cuts off bei lag 2 und ACF tails off.
# Seasonal component:   ACF cuts off bei  lag 12 und PACF tails off bei lags 12, 24, 36, ...
# => daraus folgendes Modell
sarima(unemp, p = 2, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 12)


## Von ARIMA zu SARIMA per Diagnostik
plot(diff(chicken)) 
acf2(diff(chicken), max.lag = 60)
# sample ACF and PACF besagt AR(2), da PACF cut-off nach lag 2 und ACF tails off
sarima(chicken, p = 2, d = 1, q = 0) 
# => ARIMA(2,1,0) nicht so gut
# ACF der Residuden hat seasonal component übrig => SAR(1) Komponente fitten
sarima(chicken, p = 2, d = 1, q = 0, P = 1, D = 0, Q = 0, S = 12) # 
# => SARIMA(2,1,0,1,0,0,12) besser


## Modell optimieren
d_birth <- diff(birth)
plot(birth)
acf2(d_birth, max.lag = 60)           # Seasonal Persistence
dd_birth <- diff(d_birth, lag = 12)   # seasonal differencing durchführen 
acf2(dd_birth, max.lag = 60) 
# legt SARIMA(0,1,1)x(0,1,1)_12 nahe
sarima(birth, p = 0, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
# => Reiduen nicht White-Noise => zusätzlichen AR Term um restliche Korrelation zu erklären
# Anderes Modell => hinzufügen von non-seasonal AR p=1
sarima(birth, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
# => besser


### Forecasting
## Model fitten & Diagnostik
sarima(unemp, p = 2, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 12)
# Forecast für 3 Jahren
sarima.for(unemp, n.ahead = 36, p = 2, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 12)
# => forcat kann viel der seasonal variance in den original daten replizieren

## Model fitten & Diagnostik
sarima(chicken, p = 2, d = 1, q = 0, P = 1, D = 0, Q = 0, S = 12)
# Forecast für 5 Jahre
sarima.for(chicken, n.ahead = 60, p = 2, d = 1, q = 0, P = 1, D = 0, Q = 0, S = 12)


#### TBATS
# Automatisches Verfahren, welches Komponenten aus anderen Modellen verwendet
# T = Trigonometric terms für saisonalität
# B = Box-Cox für unhomogene Varianz
# A = ARMA Fehler für kurzzeite dynamiken
# T = Trend (möglicherweise gedämpft)
# S = Seasonal (kann mehrere enthalten) 
autoplot(gas)
# TBATS-Modell fitten
fit <- tbats(gas)
fit

## Interpretation
# TBATS(0.082, {0,0}, 0.992, {<12,5>})
# 0.082     = Box-Cox Parameter
# {0,0}     = ARMA error
# 0.992     = Damping Parameter
# {<12,5>}  = Eine Saisonalität mit 12 periods mit 5 fourier terms modelliert
#   => k = 5
# Forecast für nächten 5 Jahre
fc <- forecast(fit, h = 12 * 5)
autoplot(fc)
