# => um externe Informationen in Forecast mit einzubeziehen
# Standardmäßig verwenden ARIMA und ETS nur die Historie der Zeitreihe 
# für Forecasting, aber keine anderen Informationen. 
# => aber manchmal vorhanden 
#    z.b. Forecasting von monatlichen Verkäufen die Marketingausgaben mit einbeziehen  
# Regression: y_t = \beta_0 + \beta_1 x_{1,t} + \dots + \beta_k x_{k,t} + \varepsilon_t,
#  => Normale Regression:      \varepsilon_t ist white noise
#     Dynamische Regression:   \varepsilon_t ist ARIMA-Prozess
#                              (dadurch historische Information mit einbezogen)
library(tidyverse)
library(fpp2)
library(forecast)
data(uschange)

#### Beispiel
# Forecast von Consumption mit Income als Prädikator
# => wenn Income sinkt, dann sollte auch Consumption sinken und andersrum
autoplot(uschange[,1:2], facets = TRUE) +
  xlab("Year") + ylab("") +  ggtitle("Consumption & personal income")
# Zusammenhang visualisieren
ggplot(aes(x = Income, y = Consumption), data = as.data.frame(uschange)) + 
  geom_point() + geom_smooth(method='lm',se=F) + ggtitle("Consumption & personal income")

#### Dynamisches Regressions-Modell fitten
# xreg = Martrix von Prädikaten, welche im Modell integriert werden sollen
fit <- auto.arima(uschange[,"Consumption"], xreg = uschange[,"Income"])
fit
# => Interpretation: Consumption verändert sich um 0.20 Prozent,
#    wenn income um ein Prozent verändert.
# => ARIMA-teil beschreibt die short-term time-series dynamics


#### Werte für Regressions-Variable bei Forecast simulieren
autoplot(advert, facets = TRUE)
fit <- auto.arima(advert[, "sales"], xreg = advert[, "advert"], stationary = TRUE)
## Anstieg von Sales für den Anstieg um eine Einheit von advertising
(salesincrease <- coefficients(fit)[3])
## Simulieren der nächsten 6 Monate mit jeweils 10 Einheiten von Marketing
fc <- forecast(fit, xreg = rep(10, 6))
autoplot(fc) + xlab("Month") + ylab("Sales")
# vs.. 20 Einheiten pro Monat
fc <- forecast(fit, xreg = rep(20, 6))
autoplot(fc) + xlab("Month") + ylab("Sales")


#### Quadratisches Regression-Modell mit ARMA-Fehlern
# Strom-Nachfrage als Funktion von Temperatur
# => viel Storm bei kalt wegen Heizung und viel Strom bei heiss wegen Klimaanlage
ggplot(aes(x = Temperature, y = Demand), data = as.data.frame(elecdaily)) + 
  geom_point() + geom_smooth(method='lm',se=F,formula = y ~ poly(x,2)) 

autoplot(elecdaily[, c("Demand", "Temperature")], facets = TRUE)
## Matrix der Prädikatoren
xreg <- cbind(MaxTemp = elecdaily[, "Temperature"],
              MaxTempSq = elecdaily[, "Temperature"]^2,
              Workday = elecdaily[, "WorkDay"])

## Modell fitten
fit <- auto.arima(elecdaily[, "Demand"], xreg = xreg)

# Forecast für einen Tag, bei Temperatur = 20 und Arbeitstag = 1
forecast(fit, xreg = cbind(20, 20^2, 1))



#### Dynamische harmonische Regression
# ETS und ARIMA ungeeignet für längere saisonale perioden mit 
#  z.B. wöchentlich mit Länge mit 52 - besser für: 4(quartal) oder 12(monatlich)
#  Deshalb: harmonische Regression, welche sinus und cosinus 
#           verwendet um saisonalität zu modellieren
#  => je höher k desto komplexere Muster können modelliert werden 
#     (k = 1 => einfache sinus-kurve)
#  => durch AIC bestimmen 
#
# auto.arima() dauert sonst sehr lange bei langen Zeitreihen => fourier dann besser
# ARIMA saisonalität sollte ausgestellt werden

## Dynamisches harmonishes Regressionsmodell mit ARIMA-Fehlern fitten
# Verschiedene Werte für k ausprobieren
fit <- auto.arima(auscafe, xreg = fourier(auscafe, K = 1), seasonal = FALSE, lambda = 0)
fit %>% forecast(xreg = fourier(auscafe, K = 1, h = 24)) %>%  autoplot() + xlim(2008,2020) + ylim(1.6, 5.1)
# => kein guter Fit
fit <- auto.arima(auscafe, xreg = fourier(auscafe, K = 2), seasonal = FALSE, lambda = 0)
fit %>% forecast(xreg = fourier(auscafe, K = 2, h = 24)) %>%  autoplot() + xlim(2008,2020) + ylim(1.6, 5.1)
# => besser
fit <- auto.arima(auscafe, xreg = fourier(auscafe, K = 5), seasonal = FALSE, lambda = 0)
fit %>% forecast(xreg = fourier(auscafe, K = 5, h = 24)) %>%  autoplot() + xlim(2008,2020) + ylim(1.6, 5.1)
# => viel besser


## Mehrfache Saisonalität 
# Hier: halbstündige Messungen
#       tägliche Saisonalität:      24 * 2 = 48 
#       wöchentliche Saisonalität:  7 * 48 = 336 

# Harmonische regression mit Ordnung 10 für die zwei verschiedene Saisonalitäten
fit <- tslm(taylor ~ fourier(taylor, K = c(10, 10)))

# 20 Tage forecasten
fc <- forecast(fit, newdata = data.frame(fourier(taylor, K = c(10, 10), h = 20 * 48)))
autoplot(fc)
checkresiduals(fit) # => 
