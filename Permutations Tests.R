########## PERMUATATIONS-TESTS 
library(dplyr)
library(ggplot2)
library(NHANES)

#### Zum erstellen von Permutationen
rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1){
  n <- nrow(tbl)
  i <- unlist(replicate(reps, sample.int(n, size, replace = replace), simplify = FALSE))
  rep_tbl <- cbind(replicate = rep(1:reps,rep(size,reps)), tbl[i,])
  group_by(rep_tbl, replicate)
}

#### Daten betrachten & Subset bilden
?NHANES         # American National Health and Nutrition Examination surveys
names(NHANES)   # Subset the data: homes
homes <- NHANES %>%
  select(Gender, HomeOwn) %>%
  filter(HomeOwn %in% c("Own", "Rent"))
homes <- droplevels(homes)
levels(homes$HomeOwn)
glimpse(homes) # => male/female; Own/Rent


#### HYPOTHESEN-TEST:
# Beispiel:
# Zu zeigen: Verhältnis Besitzen/Mieten bei Mann und Frau ist verschieden
#
# H_0: Das Verhältnis von Besitzen/Mieten ist bei Frauen und Männern gleich
# H_A: Es existiert eine Unterschied in Besitz/Miet Verhältnis zwischen Männern und Frauen
#
# Statistik = Differenz der Verhältnisse von Besitzen/Mieten bei Männer bzw. Frauen 
# Bsp: statistik =  p^_female - p^_male = 1/3 - 1/3 = 0 

## Beobachteten Wert berechnen 
# => Differenz der Verhältnisse von Owned/Rent bei Frau und Mann
# Bsp: Verhältnis Owned/Rented bei Mann: 0.66..
#      Verhältnis Owned/Rented bei Frau: 0.65..
#      Differenz: 0.007.. => observed Value
observed_diff_in_mean <- homes %>%
    group_by(Gender) %>% 
    summarize(m = mean(HomeOwn == "Own")) %>% # female = 0.66.., male = 0.65...
    summarize(diff(m)) # 0.66.. - 0.65
(observed_diff_in_mean <- observed_diff_in_mean[[1]])  # -0.007828723


## Erzeugen eines Null-Samples
homes %>%
  # Permutieren von Homeown  => simuliert Null-Hypothese
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%  
  # Einteilen nach Gender
  group_by(Gender) %>%
  # Berechnen der Mittelwerte für Mann und Frau unter der Werte unter der Null-Hypthose 
  # Mittelwert = Proportion => boolean-vector der auf 1/0 gecastet
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own"))%>%
  # Voneinander abziehen
  summarize(diff_perm = diff(prop_own_perm)) # => Statisktik


## Erzeugen der Null-Verteilung
# Die Verteilung der Statistik unter der Null-Hypthese erlaubt es
# abzuschätzen, ob die beobachten Daten inkostent zur Null-Hypthose sind. 
# (=> dann könnte man die Null-Hypthese ablehnen)
homeown_perm <- homes %>%
  # Resampling um "1000 neue Beobachtungen zu bekommen"
  rep_sample_n(size = nrow(homes), reps = 1000) %>%
  # Permutieren der HomeOwnSpalte - simulieren der Null-Hypothese 
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%
  group_by(replicate, Gender) %>%
  # Für jede Permutation die Statistik für Daten unter Null-Hypothese berechnen
  # Zuerst Mittelwert für Mann und Frau, wobei
  # Mittelwert = Proportion => durch mean von boolean-vector der auf 1/0 gecastet
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own")) %>%
  # Dann die beiden Mittelwerte voneinenader abziehen 
  # = Differenz der Verhälntisse von male-female (zwei Zeilen die voneinander abgezogen werden)
  summarize(diff_perm = diff(prop_own_perm))

# Null-Verteilung als Dotplot
ggplot(homeown_perm, aes(x = diff_perm)) +  geom_dotplot(binwidth = .001)
# Null-Verteilung als Density mit beobachteten Wert
ggplot(homeown_perm, aes(x = diff_perm)) + geom_density() +
  geom_vline(aes(xintercept = observed_diff_in_mean), col = "red")

# Verleich der statistiken der permutationen mit der beobachteten Differenz
observed_diff_in_mean
quantile(homeown_perm$diff_perm,c(0.025,0.975))
# => im 95% Konfidenzintervall?
smaller = count(subset(homeown_perm, subset = diff_perm <=  observed_diff_in_mean) )
print(paste(smaller,"Verhältnisse unter der Null-Hypthose sind kleiner als der beobachtete Wert."))
# => Es existiert keine Evidenz, dass die Daten inkonstent mit der Null-Hypothese sind       
#    (= wäre ein normaler Wert unter der Null-Hypothese)
# ===> Die Null-Hypthose kann nicht verworfen werden



#### Hypothesen-Test mit p-Wert Ermittlung: 
# Randomisiert: erlaubt kausalen Schluss
# Sample ist nicht Random: Keine Generalisierung möglich
### Beispiel: Gender Discrimination
# Randomisiert:        nur der Name des Bewerbers ist bei Bewerbungen anders anders und zufällig zugewiesen
# Nicht Randomisiert:  Manager die entschieden, waren nicht zufällig aus allen Mangern ausgewählt - kontext: management training
path <- file.path("~","R/data","disc.csv")
disc <- read.csv(path,stringsAsFactors = TRUE,sep=";") # normale csv 

disc %>%
 select(sex, promote) %>%
 table
# => Permutation nicht auf Basis der Zahlen der Kreuztabelle, sondern Tabelle mit den Beobachtungen

### Frage kann dies durch Zufall passiert sein?
# H0: Gender und Promotion sind sind unrelated variables. (besteht kein Link - ist also unabhängi von Geschlecht)
# HA: Männer werden mit höherer Wahrscheinlichkeit promoted als Frauen

# Verhältnis promoted vs nicht promoted von Mann und Frau
disc %>%
  group_by(sex) %>%
  summarize(promoted_prop = mean(promote == "promoted"))
# 87.5% der Männer und 
# 58.3% der Frauen wurden promoted

disc_perm <- disc %>%
  rep_sample_n(size = nrow(disc), reps = 1000) %>%
  mutate(prom_perm = sample(promote))  %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted"))  %>%
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female
# => alternativ hätte man man statt diff - also p^-p, 
#    das Verältnis der Verhältnisse nehmen können: p^/p

ggplot(disc_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = diff_orig), col = 'red')

quantile(disc_perm$diff_perm, c(.05, .95, .99)) # -0.2083333  0.2083333 0.2916667 
disc_perm$diff_orig[1] # 0.2916667 (beobachtete Wert)
# 5% der Null-Observations sind < -0.2083333
# 95% der Null-Oberservation sind < 0.2083333
# => beobachtete Wert ist größer als 95% der Null-Statistik
# ===> es gibt Evidenz, dass die Promotion-Rate von Frauen verschieden ist, 
#      können aber nicht, sagen ob die durch Diskrimierung oder andere Faktoren kommt
#      (Bei Wahl p=0.01, gilt dies nicht) 

### Kritische Region 
# sind die Quantile die Tails beschreiben
# => bestimmt, welche Statistiken mit der Null-Hyothese konsistent sind
disc_perm %>% 
  summarize(q.05 = quantile(diff_perm, p = 0.05),
            q.95 = quantile(diff_perm, p = 0.95),
            q.99 = quantile(diff_perm, p = 0.95))
      
### P-Wert
# Die Wahrscheinlichkeit, unter Annahme der Null-Hypothese, Daten zu beoachten die
# mindestens so extrem sind wie tatäschlich beobachteten Daten
# => Wahrscheinlichkeit, dass unter der Null-Annahame (kein Unterschied) die Differenz von 0.2917 oder größer beobachtet wird
#    hier: p=0.018

## Berechnen
## ONE-SIDED: haben männer eine höhere Wahrscheinlichkeit 
disc_perm %>%
  summarize(mean(diff_orig <= diff_perm))
## TWO-SIDED: besteht überhaupt ein Unterscheid in den Promotion-Rates (=> doppelter Wert)
disc_perm %>%
  summarize(mean(diff_orig <= diff_perm) * 2)


#### Hyothesen-Tests & Fehlerarten
# H0: Hinweis Geld zu sparen hat keinen Einfluss auf Kaufentscheidung
# HA: Hinweis Geld zu sparen, wird die Wahrscheinlichkeit des Kaufs verringern
# KAUSAL wegen randomisiert!! Studenten zufällig zu treatment/ctrl-Gruppe zugewiesen ist kausaler Schluss möglich.
# NICHT GENERALISIERBAR: Kein Random sample => nur Studenten
path <- file.path("~","R/data","dvds.csv")
opportunity <- read.csv(path,stringsAsFactors = FALSE,sep=';') # normale csv 

opportunity %>%
  select(decision, group) %>%
  table()

# Verhältnisse anzeigen => 20% Unterschied -> Wahrscheinlich durch Zufall möglich?
opportunity %>%
  group_by(group) %>%
  summarize(buy_prop = mean(decision == 'buyDVD'))
ggplot(opportunity, aes(x = group, fill = decision)) +geom_bar(position = "fill")

# Permutieren
opp_perm <- opportunity %>%
  rep_sample_n(size = nrow(opportunity), reps = 1000) %>%
  mutate(dec_perm = sample(decision)) %>%
  group_by(replicate, group) %>%
  summarize(prop_buy_perm = mean(dec_perm == "buyDVD"),
            prop_buy = mean(decision == "buyDVD")) %>%
  summarize(diff_perm = diff(prop_buy_perm),
            diff_orig = diff(prop_buy))  # treatment - control

ggplot(opp_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth = .005) +
  geom_vline(aes(xintercept = diff_orig), col = 'red')

# P-Wert berechnen
opp_perm %>%
  summarize(mean(diff_perm <= diff_orig))
# => Hinweis führte dazu, dass Studenten mit geringer Wahrscheinlichkeit DVD kauftem
# (Kausal wegen randomisierung, aber nicht generalisierbar, weil nur studenten)

