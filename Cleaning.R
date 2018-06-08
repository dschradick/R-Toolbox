########## CLEANING
# Tidy Data:
# 1. Beobachtungen als Reihen
# 2. Variablen als Spalten
# 3. Ein Typ von Beobachtung pro Tabelle
library(tidyverse)

#### Daten einlesen
# na für na-wert darstellung in datei
# problems() um parsing probleme auszugeben
bmi <- read_csv("~/Documents/Data/bmi_clean.csv", na=c('-1'))
problems(bmi) 

## NA Werte gleich beim einlesen konvertieren
string <- "Hallo,Daniel,-1\nHallo,Joana,2"
(y <- read_csv(string, col_names=F, na=c('-1')))


#### Übersicht
dim(bmi)
str(bmi)
glimpse(bmi)
summary(bmi)
colnames(bmi)
    
#### Missing Values
## Erkennung
is.na(mtcars)                           # Wo gibt es missing values?
sum(is.na(mtcars))                      # Anzahl der NA Werte
any(is.na(mtcars))                      # Gibt es überhautpt missing values?
summary(mtcars$mpg)                     # Anzeigen der Missing values einer Spalte
summary(mtcars)                         # Anzeigen der Missing values in allen Spalten
## Behandlung
indices <- which(is.na(mtcars$cyl))     # Indizes finden und dann durch Wert ersetzen
mtcars[indices,] <- mean(mtcars$cyl)    # durch Wert ersetzen
mtcars$name[mtcars$name == ""] <- NA    # Leere Strings durch NA ersetzen
complete.cases(mtcars)                  # Welche Reihen haben keine Missing Values
na.omit(mtcars)                         # Enternung aller Reihen mit Missing Values
# Einfach entfernen
mtcars <- mtcars[complete.cases(mtcars), ]

#### Typ-Konvertierung
class(2)
as.character(2)
as.numeric("2")
as.factor("Category")
as.logical("true")
# tolower("Hallo")
# Datum => siehe lubridate.R 

#### Value Mapping
# => um daten lesbarer zu machen
mtcars$am <- plyr::mapvalues(mtcars$am, from=c(0,1), to=c('manual','automatic'))


### Wide => Long
# Zusammenführen von spalten zu Key-Value Pairs
# - = welche spalten soll nicht transformiert werden (= im neuen df als normale Spalte!!)
# (= alle die nicht auf key-value gemapped werden sollen - 
#  VORSICHT: häufig ist vergessene nicht ausgeschlossen Spalte unten im df)
# => komplett zu entfernenden Spalten vorher entfernen
# gather(data, key = "key", value = "value", ...   , 
# wobei ... : A selection of columns. If empty, all variables are selected. 
#             You can supply bare variable names, select all variables between 
#             x and z with x:z, exclude y with -y.
bmi$other_col <- 1:length(bmi)
head(bmi)
tail((bmi_long <- gather(bmi, key=year, value = bmi_val)))
tail((bmi_long <- gather(bmi, key=year, value = bmi_val, -Country)))
(bmi_long <- gather(bmi, key=year, value = bmi_val, -Country, -other_col))
# nicht gewünschte Spalte vorher ausschliessen
bmi_small <- bmi %>% select(-other_col)
(bmi_long <- gather(bmi, key=year, value = bmi_val, -Country))
tail(bmi_long)

### Long => Wide
# Auseinander ziehen von Key-Value Pairs zu Spalten
spread(bmi_long, year, bmi_val)

### Trennen einer Spalte
# Eine Spalte in mehrere Spalten
separate(bmi_cc, col = Country_ISO, into = c("Country", "ISO"), sep = "/")


### Zusammenfühen von Spalten
# von mehrerern Spalten in eine
# Apply unite() to bmi_cc_clean
(bmi_cc <- unite(bmi_cc_clean, Country_ISO, Country, ISO, sep = "-"))


