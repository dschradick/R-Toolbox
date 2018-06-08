########## LINEARE MODELLIERUNG - SIGNAL PARTIONIERUNG & MEHRERE MODELLE
# Grundsätzliches Problem & Herangehensweise:
# PROBLEM: 
# Starkes Signal, welches es schwer macht subtilere Muster zu erkennen
# LÖSUNG: 
# 1. Rausseparieren des starken Signals durch lineares Modell, welches Signal beschreibt
# 2. Betrachten der Residuen = was übrig bleibt = subtilere Muster
# INTERPREATION:
# Residual = 1 bedeutet, dass y um eine Einheit geringer ist als die
#                        Vorhersage, welche nur auf rausseparierter Variable basierte 
library(tidyverse)
library(nycflights13)
library(broom)

#### modelr
# wrapper damit base R modeling Funktionen mit %>% benutzt werden können
library(modelr)  

# Dataset
sim1
ggplot(sim1, aes(x, y)) + geom_point()

# Test-Model
sim1_mod <- lm(y ~ x, data = sim1)
mod2 <- lm(y ~ x, data = sim2)


#### MODELL VISUALSIEREN
grid <- sim1 %>% data_grid(x)                 # X Werte erzeugen
grid <- grid %>% add_predictions(sim1_mod)    # Predictions erzeugen

## PREDICTIONS VISUALIESIEREN
# JEDE beliebige unterliegende Funktion - nicht nur linear!!!!
## Linearer Funktion
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size=1) +
  geom_point(aes(y = pred), data = grid, colour = "red", size=3)

## Kategorische Daten
grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2)
ggplot(sim2, aes(x)) +
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), color = "red", size=4)



### RESIDUEN VISUALISIEREN
# => was hat das Modell nicht erfasst
sim1 <- sim1 %>% add_residuals(sim1_mod)   # Residuen hinzufügen
# => orginalem Datenset anstatt Grid, weil y Werte benötigt

## Residuen-Verteilung Frequency-Plot (als alterntive zu Histogram)
ggplot(sim1, aes(resid)) + geom_freqpoly(binwidth = 0.5)
## Residuals vs. Predictor Plot
ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()



### INTERAKTIONEN
# * :  Interaktion als auch einzeln im Modell
# => y ~ x1 * x2  wird zu y = a_0 + a_1 * a1 + a_2 * a2 + a_12 * a1 * a2.
# 
## Kategorisch und kontinuierlich
sim3
ggplot(sim3, aes(x1, y, color=x2)) +
  geom_point()

# Zwei mögliche Modelle
mod1 <- lm(y ~ x1 + x2, data=sim3)  # als jeweils unabhänige Variable
# => parallel slopes model
mod2 <- lm(y ~ x1 * x2, data=sim3)  # fitten die Interaktion + einzeln
# => unteschiedlich Slopes
summary(mod1); summary(mod2)

## Predictions der Modelle vergleichen
grid <- sim3 %>%
  data_grid(x1, x2) %>%
  gather_predictions(mod1, mod2)

ggplot(sim3, aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~ model)

## Residuals der Modelle bzgl. der Kategorien vergleichen
sim3 <- sim3 %>%
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, color = x2)) +
  geom_point() +
  facet_grid(model ~ x2)
## => in mod1 wurde ein Pattern in b, c und d nicht erfasst!!!


### TRANSFORMATIONEN
## Transformationen
#  I() benutzen, wenn Transformation +, *, ^, - enthält
#  y ~ x + I(x^2)   =>  y = a_1 + a_2 * x + a_3 * x^2
#  y ~ x + x^2      =>  Interaktion mit sich selbst was dasselbse wie x ist
model_matrix(mtcars, mpg ~ disp + disp^2) 
model_matrix(mtcars, mpg ~ disp + I(disp^2))


##### MODELL ERSTELLUNG
library(nycflights13)
library(lubridate)



### CONFOUNDER
# Niedriger Qualität mit hohem Preis aufgrund von Confounder weight(carat)
# => carat wichtigster Predictor für Preis 
# => low-quality diamonds sind meist schwerer
# Rausseparieren des Effekts von carat um Effekt von anderne Variablen sichtbar zu machen

### Untersuchen der Zusammenhänge mit Boxplots
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()
# => überraschend: diamanten mit niedriger Qualität haben hohe Preise
#    z.B  Farbe J und schlechteste clarity I1 
# ==> confounding

## Hexplot statt Scatterplot bei vielen Observations
ggplot(diamonds, aes(carat, price)) + geom_hex(bins = 50)
# Vorverarbeiten der Daten um Zusammenhang von price und carat besser untersuchen zu können
diamonds2 <- diamonds %>%
  filter(carat <= 2.5) %>%
  mutate(lprice = log2(price), lcarat = log2(carat))
ggplot(diamonds2, aes(lcarat, lprice)) +
  geom_hex(bins = 50)
# => lineare Muster - besser!

## Neues Modell auf transformierten Daten
mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)
# Vorhersage und Rücktransformation für originalen Plot
grid <- diamonds2 %>%
  data_grid(carat = seq_range(carat, 20)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(mod_diamond, "lprice") %>%
  mutate(price = 2 ^ lprice)
ggplot(diamonds2, aes(carat, price)) +
  geom_hex(bins = 50) +
  geom_line(data = grid, color = "red", size = 1)
## Residuen haben nun keinen linearen Trend mehr
diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond, "lresid")
ggplot(diamonds2, aes(lcarat, lresid)) +
  geom_hex(bins = 50)
## Versus altes Modell
mod_diamond_normal <- lm(price ~ carat, data = diamonds)
diamonds <- diamonds %>%
  add_residuals(mod_diamond_normal, "resid")
ggplot(diamonds, aes(carat, resid)) +
  geom_hex(bins = 50)

#### NACH RAUSSEPARIEREN DES CONFOUNDERS!!!!
# durch verwendung der Residuen statt Price
ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()
# => Nachdem Effekt des Confounders entfernt
#    sehen Beziehungen wie erwartet aus
# INTERPREATION der y-Achse = Residuen
# Was sagen die Residuen aus und wie skaliert? 
# Residual = 1 bedeutet, dass lpric e1 unit geringer war als die
#                        Vorhersage, welche nur auf Gewicht basierte 
# 2^(-1) = 1/2 => also Punkte mit 1 sind das halbe des erwarteten Preis 
# und Residuen mit dem Wert 1 sind das doppelte des vorhergesagten Preis


### KOMPLEXERES MODELL untersuchen & CONFOUNDER
# Mehrere Prädikatoren
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)
mod_diamond2_without_confounder <- lm(lprice ~ color + cut + clarity, data = diamonds2)

## Untersuchen 
# Problem: mehrere Variablen schwer zu visualiseren
# => da alle unabhängig, können sie einzeln untersucht werden
# Cut
grid <- diamonds2 %>%
  data_grid(cut, .model = mod_diamond2) %>%
  add_predictions(mod_diamond2)
ggplot(grid, aes(cut, pred)) +
  geom_point()
# Wenn Confounder nicht explizit drin wäre
grid <- diamonds2 %>%
  data_grid(cut, .model = mod_diamond2_without_confounder) %>%
  add_predictions(mod_diamond2_without_confounder)
ggplot(grid, aes(cut, pred)) +
  geom_point()

## Residuen
diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond2, "lresid2")
ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_hex(bins = 50)
# => besser, aber immernoch einzelne Ausreisse => betrachten!

## Untersuchen der Ausreisser / schlecht erklärbaren Punkt
diamonds2 %>%
filter(abs(lresid2) > 1) %>%
  add_predictions(mod_diamond2) %>%
  mutate(pred = round(2 ^ pred)) %>%
  select(price, pred, carat:table, x:z) %>%
  arrange(price)


#### Beispiel Flugdaten
# Zielvariable: Number of flights

### Daten betrachten
daily <- flights %>%
  mutate(date = make_date(year, month, day)) %>%
  group_by(date) %>%
  summarize(n = n())

ggplot(daily, aes(date, n)) +
  geom_line()
# Problem: Langzeittrend schwer erkennbar
#  => starker day-of-week effect dominiert subtilere Muster

## Day-Of-Week Effekt
daily <- daily %>%
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(wday, n)) +
  geom_boxplot()
# => weniger Flüge am Wochenende

## ENTFERNEN DES TAG-ZU-TAG MUSTER
# Durch Modell
# 1. Modell erzeugen & Predictions mit Originaldaten überlagern
# 2. Berechnen und visualisieren der Residuals
# 
## 1. Modell erzeugen & Predictions überlagern
mod <- lm(n ~ wday, data = daily)
grid <- daily %>%
  data_grid(wday) %>%
  add_predictions(mod, "n")
ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red", size = 4)#
#
## 2. Berchnen und visualisieren der Residuals
daily <- daily %>%
  add_residuals(mod)
daily %>%
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line() + 
  geom_smooth(se = FALSE, span = 0.20)
# Y-Achse: y-axis: sieht Abweichung von erwarteter Anzahl von Flügen,
#                  bei gegebenen Tag der Woche
# => Effekt von Day-of-Week entfernt - erlaubt nun subtilere Muster zu sehen


# Modell scheint ab Juni nicht richtig zu sein
# => Ursache sehen in Plot mit einer Linie pro Tag
ggplot(daily, aes(date, resid, color = wday)) +
  geom_ref_line(h = 0) +
  geom_line()
# Zudem einige Tage mit weit weniger Flügen als erwartet
daily %>%
  filter(resid < -100)
# => Neujahr, 4. Juli, Thanksgiving und Christmas

## LANGZEIT-TREND über das Jahr gesehen
daily %>%
  ggplot(aes(date, resid)) + 
    geom_ref_line(h = 0) + 
    geom_line(color = "grey50") + 
    geom_smooth(se = FALSE, span = 0.20)
# => anscheinend jahrezeiten-abhängig

## Samstag Effekt
daily %>%
  filter(wday == "Sat") %>%
  ggplot(aes(date, n)) +
    geom_point() +
    geom_line() +
    scale_x_date(
      NULL,
      date_breaks = "1 month", date_labels = "%b")
# => offentsichlich Sommerferien

### Jahrezeit
## Jahrezeit-Variable einführen
term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall")
  )
}
daily <- daily %>%
  mutate(term = term(date))
daily %>%
  filter(wday == "Sat") %>%
  ggplot(aes(date, n, color = term)) +
  geom_point(alpha = 1/3) +
  geom_line() +
  scale_x_date(
    NULL,
    date_breaks = "1 month", date_labels = "%b")

# Wie beeinflusst Jahreszeit das Day-of-week Muster
daily %>%
  ggplot(aes(wday, n, color = term)) +
  geom_boxplot()
# => starke Variation zwischen den Jahrezeiten
# ==> beide fitten - verbessert Modell
mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)
daily %>%
  gather_residuals(without_term = mod1, with_term = mod2) %>%
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha = 0.75)
# => immernoch nicht optimal 
# ==> genauer betrachten durch überlagern der Predictions mit orginal Daten
grid <- daily %>%
  data_grid(wday, term) %>%
  add_predictions(mod2, "n")
ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red") +
  facet_wrap(~ term)
# => Modell bestimmt mean-value - aber starke Ausreisser, 
#    welche das Ergebnis verzerren
# ==> Robuste Regression - robust gegen Ausreisser
mod3 <- MASS::rlm(n ~ wday * term, data = daily)
daily %>%
  add_residuals(mod3, "resid") %>%
  ggplot(aes(date, resid)) +
  geom_hline(yintercept = 0, size = 2, color = "white") +
  geom_line() +
  geom_smooth(se = FALSE, span = 0.20)
# => einfacher den Langzeit-Trend und die positiven/negativen Outlier zu erkennen



## Jahreszeit - alternativer Ansatz: Natural Splines
# vorher: Domain knowledge benutzt
# jetzt: flexibleres Modell benutzen

## Linearer Trend nicht adäquat Daten nicht gut 
#   => Natural Spline verwenden
library(splines)
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)
daily %>%
  data_grid(wday, date = seq_range(date, n = 13)) %>%
  add_predictions(mod) %>%
  ggplot(aes(date, pred, color = wday)) +
  geom_line() +
  geom_point()
# => sieht gut den Trend an Samstagen

daily %>%
  add_residuals(mod, "resid") %>%
  ggplot(aes(date, resid)) +
  geom_hline(yintercept = 0, size = 2, color = "white") +
  geom_line() +
  geom_smooth(se = FALSE, span = 0.20)

## Globale Funktion zur Variablen Transformation
# damit einheitlich, wenn an mehreren Stellen benutzt
compute_vars <- function(data) {
  data %>%
    mutate(
      term = term(date),
      wday = wday(date, label = TRUE)
    ) 
}

#### MEHRERE MODELLE
# Beispiel:  
# “How does life expectancy (lifeExp) change over time (year) 
#  for each country (country)?”
library(gapminder)
gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)
# => einige haben anderes Muster
# Auch hier: starkes Muster, welches es problematisch macht die subtileren Muster zu sehen
# Lösung: lineares Modell, welches starkes Muster beschreibt für jedes Land & überprüfen was übrig bleibt in den Residuen


#### MEHRERE MODELLE
# Idee:
# - mehrere einfache Modelle um komplexere Datensätze zu verstehen
# - list-columns zur Speicherung von beliebigen Strukturen in Dataframes
# - broom um Modelle in tidy-data zu transformieren, um die Resultate der Modelle zu speichern

# Frage: 
# "How does life expectancy (lifeExp) 
#  change over time (year) for each country (country)?"

## Zuerst beschreibenden Plot
library(gapminder)
gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)
# => schwer zu sehen: Haupttrend scheint zu sein, dass Lebenserwartung steigt
#    Aber bei einigen Ländern offensichtlich nicht
# Auch hier: starkes Signal, welches es problematisch macht die subtileren Muster zu sehen
# Lösung: lineares Modell, welches starkes Signal beschreibt für jedes Land & überprüfen was übrig bleibt in den Residuen


## Für ein Land: Original-Daten = Linearer Trend + überbleibendes Muster
nz <- filter(gapminder, country == "New Zealand")
nz %>%
  ggplot(aes(year, lifeExp)) +
  geom_line() +
  ggtitle("Full data = ")
nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>%
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle("Linear trend + ")
nz %>%
  add_residuals(nz_mod) %>%
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, color = "white", size = 3) +
  geom_line() +
  ggtitle("Remaining pattern")

#### Nun Modell für jedes Land fitten

### NESTING
# Nested Dataframe benutzen
gapminder
gapminder %>% group_by(country, continent) # vs..
gapminder %>% group_by(country, continent) %>% nest() 
# => erzeugt eine Reihe pro Gruppe 
#    Spalte "data" enthät die Dataframe
by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()
  
# Zugriff auf data spalte
by_country$data[[1]]  # => jede Reihe ist eine Gruppe (vs Beobachtung)

## Funktion zum fitten eines(!) Modells
# => kann dann auf durch map auf alle nested dataframes angewendet werden
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

# Dataframes sind einer Liste, deshalb purrr::map() 
models <- map(by_country$data, country_model)
head(models)

## Zentrale Verwaltung: ein Dataframe - alles reinpacken 
# Alles direkt nebeneinander, ist Stärke von Dataframe für diese Vorgehensweise (vs einzelne Objekte)
# => keine synchronisation notwendig, wenn man filter() oder arrange() aufruft
by_country <- by_country %>%
  mutate(model = map(data, country_model))
by_country
by_country %>% filter(continent == "Europe")
by_country %>% arrange(continent, country)
# => durch ein zentralen Dataframe kein reordern von indizes

## UNNESTING
# Um add-residuals() aufzurufen, muss es 
# auf jedes nested model-data Paar angewandt werden
by_country <- by_country %>%
  mutate(resids = map2(data, model, add_residuals))
by_country
# Unnesting um plots der dataframes zu machen
resids <- unnest(by_country, resids) 
resids
resids %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) +
  geom_smooth(se = FALSE)
# Faceting nach country gibt mehr Informationen
resids %>%
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) +
  facet_wrap(~continent)
# => Afrika zeigt große Residuen

### Goodness of fit statt grafisch
# Broom für Metriken für Model Qualität verwenden
## Für ein Land
glance(nz_mod) 
## Für jedes Land
model_quality <- by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop=T) 
# Drop => ander list-columns nicht mit reinnehmen
model_quality
## Nach r^2 sortieren
model_quality %>%
  arrange(r.squared)
## Fits grafisch anzeigen
model_quality %>%
  ggplot(aes(continent, r.squared,color=continent)) +
  geom_jitter(width = 0.5)
## Besonders schlechte fits anzeigen
bad_fit <- filter(model_quality, r.squared < 0.25)
gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(aes(year, lifeExp, color = country)) +
  geom_line()
# => main effects: HIV/AIDS und Rhuanda 
