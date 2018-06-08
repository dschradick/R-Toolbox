########## ZEITREIHEN - XTS
# EXtensive Time Series
# xts = matrix + index (time-object)
#     = matrix mit verbunden zeiten für jede Beobachtung
# Unterklasse von zoo
library(xts)
library(astsa)
library(readr)
library(PerformanceAnalytics)

#### Erzeugen
## Einfach
dates <- seq(as.Date("2014-01-19"), length = 176, by = "weeks")

x <- matrix(1:4, ncol=2, nrow=2)
idx <- as.Date(c("2016-01-01","2016-01-02"))
(X <- xts(x, order.by=idx))


## Komplexer
(dates <- as.Date("2016-01-01") + 0:699)                            # =...
(dates <- seq(as.Date("2016-01-01"), length = 700, by = "days"))
data <- rnorm(700)
X <- xts(x = data, order.by = dates)

#### Daten betrachten
str(X)
coredata(X,fmt=F)   # Daten-Komponente => Matrix
index(X)            


#### Einlesen
file <- "~/Documents/R/data/Temps.csv"
## Mit read.csv
dat <- read_csv(file)
temps <- xts(dat, order.by = as.Date(dat$Index, "%Y-%m-%d"))
temps$Index <- NULL
## mit read.zoo
temps_zoo <- read.zoo(file, header = T, index.column = 1, sep = ",", format = "%Y-%m-%d")
(temps <- as.xts(temps_zoo))
## Schreiben
write.zoo(temps_xts, sep = ",", file = file)


#### Daten-Zugriff
X[2, 2]
coredata(X,fmt=F)[2,2]

#### Subsetting
X["2016"]                              # alle Werte von 2016
X["2016-03"]                           # alle Werte von März 2016
X["2016/2016-03-22"]                   # / = von-bis 1.1.2016-22.3.2016 
x["2016-06-09/"]                       # von Datum bis Ende
X["T08:00/T10:00"]                     # Intraday wiederholend - jeden Tag von 8-10 Uhr
(lastweek <- last(temps, "1 week"))    # letze Woche der Series
                                       # weeks, months, hours,...
last(temps, 2)                         # letzte zwei Tage
last(temps, "-2 days")                 # alle ausser die ersten zwei Tage 
first(lastweek, "-2 days")             # kombinieren: alle ausser die ersten beiden tage der letzen Woche
# Ersten drei Tage der zweiten Woche von temps
first(last(first(temps, "2 weeks"), "1 week"), "3 days")

## Weiteres
X[dates]                         # Subsetting mit dates-vector
X[index(X) > "2016-01-01"]       # Alternativ
(index <- X["2016/2016-03-22", which.i = T])

#### Zuweisungen
X["2016-06-09/"] <- 0


#### Joining
x <- matrix(1:3, ncol=1, nrow=3); idx1 <- as.Date(c("2016-01-01","2016-01-02","2016-01-03"))
y <- matrix(4:6, ncol=1, nrow=3); idx2 <- as.Date(c("2016-01-02","2016-01-03","2016-01-04"))
(a <- xts(x, order.by=idx1)); (b <- xts(y, order.by=idx2))

## Spalten
cbind(a, b)                   # =...
cbind(a, b, join = "outer")   
cbind(a, b, join = "inner")   # =...
merge(a, b, join = "inner")
merge(a, b, join = "left", fill = 0)

## Reihen
rbind(a, b)
rbind(a, b, b)                     # beliebig viele

#### Missing Values
x <- matrix(1:3, ncol=1, nrow=3); idx1 <- as.Date(c("2016-01-01","2016-01-02","2016-01-03"))
a <- xts(x, order.by=idx1); a[2] <- NA
na.locf(a)                         # fill mit letzer Beobachtung
na.locf(a,fromLast = T)            # fill mit nächster Beobachtung
na.approx(a)                       # Linear Interpolation

#### Leading und Lagging
# => Vorzeichen von k andersrum als in Literatur
x <- xts(1:10,order.by=as.Date("2016-01-01")+1:10)
(lead_x <- lag(x, k = -1))         # Leading Reihe - shift nach hinten
(lag_x  <- lag(x, k = 1))          # Lagging       - shit nach vorne 
(z <- cbind(lead_x, x, lag_x))     

#### Differenzieren
## Von Hand
AirPassengers- lag(AirPassengers)
# Differenzierung erster Ordnung 12 Monate
diff(AirPassengers, lag = 12, differences = 1)

#### Letzte Beobachtung in Interval
# liefert Positionen (nicht Werte)
endpoints(temps, on = "weeks")         # geht auch mit years,...
endpoints(temps, on = "weeks", k = 2)  # jedes zweites Jahr

#### Funktion anwenden

### Period.apply
# Wöchtentliches mean
ep <- endpoints(temps, on = "weeks")
period.apply(temps[, "Temp.Mean"], INDEX = ep, FUN = mean)


### Split (in kleinere Chunks) & lapply
temps_weekly <- split(temps, f = "weeks")
temps_weekly
# Create a list of weekly means, temps_avg, and print this list
temps_avg <- lapply(X = temps_weekly, FUN = mean)
temps_avg

#### Priodizität 
# => frequenz ändern
periodicity(temps)
(x <- to.period(temps, period = "weeks"))
periodicity(x)

to.quarterly(temps, name = "t", indexAt = "firstof")
periodicity(edhec)
edhec_yearly <- to.yearly(edhec)
periodicity(edhec_yearly)  # !=...
periodicity(edhec_years)

nmonths(edhec)
nquarters(edhec)

#### Rolling Functions 
##!!!!!!
# GRUNDFORM: split-lapply-rbind
##!!!!!
## => gut danach das Erbebnis zu plotten

## Rolling Wert einer Reihe nach Monat
x <- edhec[,"Equity Market Neutral"]
edhec_years <- split(x, f = "years")  # => monatlich
# lapply benutzen, um die cumsum für jedes jahr zu berechnen
(edhec_ytd <- lapply(edhec_years, FUN = cumsum))
# Liste in ein xts objekt
edhec_xts <- do.call(rbind, edhec_ytd)
plot.xts(edhec_xts, ylim = c(0, 1))

## Rollapply
# Anwenden einer Funktin auf über Reihe rollendes Fenster 
# width = anzahl der Beobachtungen im Fenser
# Use rollapply to calculate the rolling 3 period sd of eq_mkt
edhec_years <- do.call(rbind,split(x, f = "years"))
eq_sd <- rollapply(edhec_years, 3, sd)         # Zeiteinheit der Serie = Monate
plot.xts(eq_sd)
plot.zoo(eq_sd)


#### Indexing
.index(temps)
.indexwday(temps)
## Index von Wochenende
index <- which(.indexwday(temps) == 0 | .indexwday(temps) == 6)
temps[index]


#### Time-Zones
tzone(temps) <- "Europe/Berlin"
tzone(temps)


#### Weiteres
## Eindeutiger Zeitstempel
z_unique <- make.index.unique(z, eps = 1e-4)
## Dubletten entferenen
z_dup <- make.index.unique(z, drop = TRUE)
# Beobachtungen auf die nächsten Stunden runden
z_round <- align.time(z, n = 3600)