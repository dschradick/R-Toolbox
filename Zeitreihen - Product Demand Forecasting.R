########## ZEITREIHEN - PRODUCT DEMAND
library(xts)
library(forecast)

#### Daten einlesen
file <- "~/Documents/Data/beverage_sales.csv"
dat <- read.csv(file)
dates <- seq(as.Date("2014-01-19"), length = 176, by = "weeks")
bev <- xts(dat, order.by = dates, "%Y-%m-%d")
head(bev)

#### Visualisieren
## Drei Produkte in der metropolitan area 
sales_high <- bev[,"MET.hi"]
sales_low <- bev[,"MET.lo"]
sales_special <- bev[,"MET.sp"]
# Preis des high-end product
prices_high <- bev[,"MET.hi.p"]

# Alles Verkäufe
sales_all <- sales_high + sales_low + sales_special
plot(sales_all)


#### Train / Test-set generieren
bev_train <- bev[index(bev) < "2017-01-01"]
bev_test  <- bev[index(bev) >= "2017-01-01"]
all_sales_train <- sales_all[index(sales_all) < "2017-01-01"]
all_sales_test <- sales_all[index(sales_all) >= "2017-01-01"]

all_sales_model <- auto.arima(all_sales_train)


#### Forecast
# Für erste 22 Wochen in 2017
forecast_all_sales <- forecast(all_sales_model, h = 22)
plot(forecast_all_sales)



#### MAE & MAPE
forecast_means <- as.numeric(forecast_all_sales$mean)
all_sales_test_values <- as.numeric(all_sales_test)
MAE <- mean(abs(forecast_means - all_sales_test_values))
MAPE <- 100*mean(abs((forecast_means - all_sales_test_values)/all_sales_test_values))

print(MAE)   
# => der forecast lag im schnitt um 890 getränke daneben - gut? => MAPE
print(MAPE)  
# => der forecast lag im schnitt um 17% daneben

## Forecast von test-set vs echte Daten visualisieren
forcast_dates <- seq(as.Date("2017-01-01"), length = 22, by = "weeks")
forecast_means_xts <- xts(forecast_all_sales$mean, order.by = forcast_dates)
# Plot von test-set
plot(all_sales_test, main = 'Forecast Comparison', ylim = c(4000, 8500))
# Overlayen mit dem forecast von 2017
lines(forecast_means_xts, col = "blue")

## Mit 95%-Prediction Interval
# => lower und upper haben zwei Werte - für 80% und 95% Prediction Interval
plot(all_sales_test, main = 'Forecast Comparison', ylim = c(4000, 8500))
lines(forecast_means_xts, col = "blue")

# Limit nach xts konvertieren
lower <- xts(forecast_all_sales$lower[,2], order.by = forcast_dates)
upper <- xts(forecast_all_sales$upper[,2], order.by = forcast_dates)
lines(lower, col = "blue", lty = "dashed")
lines(upper, col = "blue", lty = "dashed")


#### Preis-Elastizität
# Elastisch  :     Price Elasticity > 1
# Unelastisch:     Price Elasticity < 1
# Unit-Elastisch:  Price Elasticity = 1
#
# Log-Transform:
# Transfomiert Koeffizient von Einheit der Nachfrage nach Prozent-Änderung in Nachfrage
# bei gegebener Prozent-Änderung in Preis
log_sales_train <- as.vector(log(bev_train[,'MET.hi']))
log_price_train <- as.vector(log(bev_train[,"MET.hi.p"]))

## Modell erstellen (für Preis-Elastizität)
train_set <- data.frame(log_sales_train,log_price_train)
colnames(train_set) <- c("log_sales", "log_price")
par(xpd=FALSE)
with(train_set,plot(log_sales_train ~ log_price))
abline(lm(train_set$log_sales ~ train_set$log_price),col='red')
model_hi_sales <- lm(log_sales_train ~ log_price, data = train_set)
model_hi_sales  
# => log_price = -1.51
# => absoluter wert = 1.51 > 1 => elastisch




#### Saisonale- / Urlaubs- / Promotionale-Effekte
# Indikator-Variable für die verschiedenen mögliche Ursachen die Effekte haben könnten
par(mfrow = c(2,1), mex = 0.6, cex = 0.8)
plot(sales_high)    # Spikes?
plot(prices_high)  
# Saisonale Effekte? => Zusammenhänge schwer zu sehen - Regression
par(mfrow = c(1,1))

dates_train <- seq(as.Date("2014-01-19"), length = 154, by = "weeks")
## Valentinstag
valentine_dates <- as.Date(c("2014-02-09", "2015-02-08", "2016-02-07"))
valentine_dates_xts <- as.xts(rep(1, 3), order.by = valentine_dates)
valentine_dates_xts <- merge(valentine_dates_xts, dates_train, fill = 0)

# Neujahres-Woche
newyear_dates <- as.Date(c("2014-12-28", "2015-12-27", "2016-12-25"))
newyear_dates_xts <- as.xts(rep(1, 3), order.by = newyear_dates)
newyear_dates_xts <- merge(newyear_dates_xts, dates_train, fill = 0)

train_set <- data.frame(log_sales_train,
                           log_price_train,
                           newyear_dates_xts,
                           valentine_dates_xts)
colnames(train_set) <- c("log_sales", "log_price",'newyear','valentine')

model_full <- lm(log_sales_train ~ log_price + newyear_dates_xts + valentine_dates_xts, data = train_set)
summary(model_full) # => keine signifikant



#### Variablen in der Zukunft
# Wenn Preis mit einbezogen werden soll, ist er möglicherweise für den 
# Forecasting-Tag nicht vorher bekannt. 
# => eigenes (z.B. Arima Modell) für Preis dessen Ergebnis als Eingabe für Demand-Modell





#### Forecasting mit Regression
log_sales_test <- as.vector(log(bev_test[,"MET.hi"]))
log_price_test <- as.vector(log(bev_test[,"MET.hi.p"]))
test_set <- data.frame(log_price_test)
colnames(test_set) <- "log_price"

# Vorhersage von log von sales
preds_log <- predict(model_hi_sales, test_set)
# Konvertieren von log => normal
predictions <- exp(preds_log)

## Plotten des Forecasts
dates_test <- seq(as.Date("2017-01-01"), length = 22, by = "weeks")
predictions_regression <- xts(predictions, order.by = dates_test)
plot(predictions_regression)

sales_test <- bev_test[,"MET.hi"]

(MAPE <- 100 * mean(abs((predictions_regression - sales_test)/sales_test)))



#### Kombieren von ARIMA und Regression
# Regression verbessern = Residuen verkleinern => zusätzliche Prädikatoren
#
# Allgemein: Wenn noch Muster = Informationen in den Residuen, dann 
#            nutzt Modell die Informationen noch nicht alle Informationen in 
#            den Prädikatoren richtig aus => besseres Modell möglich
#
# Residuals zeitlich voneinander abhängig? 
# => Zeitreihen-Modelle verwenden, um die Residuen selbst zu forecasten
#
### Forecasting der Residuen

## Residuen berechnen & visusaliseren
model_resids <- residuals(model_full)
model_resids <- xts(model_resids, order.by = dates_train)
hist(model_resids)
plot(model_resids)  # => sieht schon aus wie zeitreihe

## ARIMA-Modell auf den Residuen
arima_model_resids <- auto.arima(model_resids)
summary(arima_model_resids)

## Forecasting der Residuals
forecast_resids <- forecast(arima_model_resids, h = 22)
head(forecast_resids, n = 10)

## Visualisieren des Forecasts der Residuals
dates_test <- seq(as.Date("2017-01-01"), length = 22, by = "weeks")
forecast_resids_log <- xts(forecast_resids$mean, order.by = dates_test)
plot(forecast_resids_log)  
# => Wellenform zu erwarten, da Residuen auch Wellenform hatten
# => Wellenform wird schwächer, da mit längerer Zeit 
#    der beste Forecast der Mittelwert selbst ist


### Idee: Kombinieren von ARIMA und Regression
# Zwei grundsätzliche Vorgehen um Foecasting Techniken zu kombinieren:
# 1. Transfer Funktion:   alles mathematisch in ein Modell integrieren 
#    => kombiniert die Herangehensweisen = kombierenen in der Schätzphase
# 2. Ensembling:          kombinieren der forecasts die von den jeweiligen Modellen erzeugt werden 
#                         Einfachste Möglichkeit der Kombination: Mittelwert
#   = > kombiniert nur die Ergebnisse der Methoden

### Transfer Funktion
# Kombination von zwei mathematischen Techniken in einer mathematisch:
# log(demand), log(price)
#
# Regressions-Modell: log(Y_t) = \beta_0 + \beta_1*log(X_t) + \beta_2*X_2 + ... + \epsilon_t
# AR-Modell:          \epsilon_t = \alpha_0 +  alpha_1*\epsilon_{t-1} + alpha_2*\epsilon_{t-2} + ... + \epsilon
#
# Kombination:
# log(Y_t) = log(\hat{Y_t}) + \hat{\epsilon_t}
#
# Log => Normal:
# Y_t = \hat{Y_t} + exp(\hat{\epsilon_t})

## Konvertieren der residuals von log nach normal
forecast_prediction <- exp(forecast_resids_log)

## Kombinieren der Regressions- und ARIMA-Vorhersagen
transfer <- predictions_regression * forecast_prediction

(MAE <- mean(abs(transfer - sales_test)))
(MAPE <- 100*mean(abs((transfer - sales_test)/sales_test)))



#### Ensembling
## ARIMA Model
(sales_high_arima <- auto.arima(sales_high))

## Forecasting
forecast_arima <- forecast(sales_high_arima, h = 22)
forecast_arima <- xts(forecast_arima$mean, order.by = dates_test)

## Evaluieren von ARIMA
(MAPE <- 100*mean(abs((forecast_arima_xts - sales_high)/sales_high)))

## Ensembling
# Ensemble the two forecasts together
forecast_ensemble <- (forecast_arima_xts + predictions_regression)/2

## Evaluieren
(MAE <- mean(abs(forecast_ensemble - sales_test)))
(MAPE <- 100*mean(abs((forecast_ensemble - sales_test)/sales_test)))

## Plot-Vergleich der Modelle
plot(transfer, ylim = c(1000, 3000), col='red')
lines(predictions_regression, col='green')
lines(forecast_ensemble,col='black')
lines(sales_test, col = "blue")


