########## PLYR
# Split, Apply, Combine
# ddply: Dataframe (gemäß Variable) splitten, funktion darauf anwenden, und wieder zusammenführen
# => gute kontrolle über die eingabe & ausgabe-datentypen
# => insbesondere gut für summarizing von Daten
library(plyr)
set.seed(0)

#### Testdaten 
data <- data.frame(
  year = rep(2014:2016, each = 3), 
  count = round(runif(9, 0, 20)))
str(data)

#### Mapping von Values
mapvalues(mtcars$am, from=c(0,1), to=c('manual','automatic'))

#### **ply
# (Inputtype)(Outputtype)ply
# Types: d = dataframe, l = list, a = array(+matrix)
# ddply = nimm data.frame, mach etwas damit, gebe data.frame zurück
# ddply(.data, .variables, .fun,...)


### Summarize => erzeugt aggregierten für jede Gruppe (year) data.frame
groupvars <- c("year")
ddply(data, groupvars, summarise, mean.count = mean(count))

### Tranform => modifiziert data.frame
ddply(data, "year", transform, total.count = sum(count))

### Mutate =>  wie tranform, erlaubt aber aufbauende Spalten
ddply(data, "year", mutate, mu = mean(count), sigma = sd(count), cv = sigma/mu)

### Eigene Funktion an ddply übergeben
my_func <- function(x) {
  mean.count <- mean(x$count)
  sd.count <- sd(x$count)
  cv <- sd.count / mean.count
  data.frame(cv.count = cv, mc = mean.count)
}
ddply(data, "year", my_func)


