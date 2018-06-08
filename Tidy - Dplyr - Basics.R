########## DPLYR
#### Grammatik für Datenmanipulation
# Implementiert durch 5 Verben 
# select      - wählt Spalten
# filter      - wählt Reihen
# arrange     - ordnet Reihen
# mutate      - erzeugt neue spalte auf Basis anderer
# summarize   - berechnet summary-statistics
# => liefern jeweils neues Datenset, keine quotes benötigt
library(dplyr)
library(hflights)


#### Datentyp tibble
# Erweiterter data.frame (davon abgeleitet)
# tibble() vs data.frame(): macht weniger! 
# => erzeugt keine row.names() oder ändert nicht typ des inputs (z.B konvertiert strings nicht zu factors)
# => besser mit zu arbeiten und bessere Darstellung (z.B. passt sich Fenster an)
hflights <- as_tibble(hflights) # konvertierung
hflights                        # bessere Darstellung
as.data.frame(hflights)         # zurückkonvertieren
## Betrachten
glimpse(hflights) # ähnlich wie str
names(hflights)   # column-names => (statt immer glimpse)
## Sinnvolles
!is.na(column)
rank(c(3,2,5,4)) # => 2 1 4 3
ifelse(ArrDelay > 10,'big_delay','small_delay')
## Mapping von Column-Inhalten
hflights$UniqueCarrier
lookUpTable <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental")
hflights$Carrier <- lookUpTable[hflights$UniqueCarrier]

## transposed tibble: tR(!)ibble
tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)


## SELECT
# Wählt Spalten aus
select(hflights, 1:5, -2,-3) # ersten 4 Spalten ohne die zweite und dritte
select(hflights, AirTime, ArrDelay, DepDelay) # Explizte Liste
select(hflights,Origin:Cancelled)  # Spalten-Slice auf Namenbasis
select(hflights,Year:DayOfWeek,ArrDelay:Diverted) # Kombiniert
# Helper Functions
select(hflights, contains("num"),starts_with("Cancel"))

## MUTATE
# Erzeugt neue Spalte
# mutate(my_df, x = a + b, y = x + c)
# =>  x kann in zweiter Zuweisung bereits verwendet werden
mutate(hflights, loss = ArrDelay - DepDelay, loss_ratio = loss / DepDelay) 
mutate(hflights, delay_type = ifelse(ArrDelay > 10,'big_delay','small_delay'))
mutate(hflights,ActualGroundTime = ActualElapsedTime - AirTime)
c2 <- mutate(hflights, Date = paste(Year,Month,DayofMonth,sep="-"))
head(c2$Date)

## FILTER
# Wählt Zeile aus
# filter(df, a > 0, b > 0)
filter(hflights, Cancelled == 1)
filter(hflights, !is.na(Cancelled)) 
filter(hflights, Cancelled == 1 & UniqueCarrier == 'AA')
filter(hflights, TaxiOut + TaxiIn > AirTime)
filter(hflights, UniqueCarrier %in% c('AA','CO'))

## ARRANGE
# Neusortieren der Daten
# arrange(df, c1, ...)  
arrange(hflights, DepDelay)
arrange(hflights, DepDelay, UniqueCarrier) 
arrange(hflights, DepDelay + ArrDelay) # muss keine einzelne Spalte sein
arrange(hflights, desc(DepDelay))      # absteigend

# SUMMARISE
# Erzeugt neue Tabelle mit Statistiken
summarise(hflights,min_dist = min(Distance),max_dist=max(Distance))
temp <- filter(hflights,!is.na(DepTime),!is.na(ArrTime))
summarise(temp,max_flight_time = max(abs(DepTime - ArrTime))) # 
# spezielle dplyr aggreationsfunktionen: n(), n_distinct(), nth(x,n)
summarise(hflights, n_flights = n(), n_canc = sum(Cancelled == 1), avg_delay = mean(ArrDelay,na.rm = TRUE))


## PIPE-OPERATOR
# Verwendet das Ergebnis der linken Seite als erstes Argument für die rechte Seite
# obj %>% func(arg2,arg3,..)
# "then" : x %>% y %>% z : Nehem x, dann mache y, dann mache z

c(1,2,3) %>% sum() # 6
hflights %>%
  mutate(diff = TaxiOut - TaxiIn) %>%
  filter(!is.na(diff)) %>%
  summarise(avg = mean(diff))

## GROUP-BY
# Gotcha: keine '' für Feldnamen wie bei pandas
# group_by(df,column_to_group, column_to_group_2)
hflights %>%
  group_by(Cancelled)
  summarise()
hflights %>%
  group_by(UniqueCarrier) %>%
  summarise(p_canc = mean(Cancelled == 1) * 100,  # Prozent der gecancelten Flüge
            avg_delay = mean(ArrDelay, na.rm = TRUE)) %>%  # 
  mutate(rank = rank(avg_delay)) %>%
  arrange(rank,avg_delay, p_canc)  
  
#### Joins & Mengen
artists <- read.csv("~/R/data/artists.csv",stringsAsFactors = F)
bands <- read.csv("~/R/data/bands.csv",stringsAsFactors = F)

### Mutating Joins
# x %>% left_join(y, by = c("x.name1" = "y.name2")) # um keynames zu setzen
left_join(artists, bands, by = c("first","last"))
right_join(artists, bands, by = c("first","last"))
inner_join(artists, bands, by = c("first","last"))
full_join(artists, bands, by = c("first","last"))

### Filtering Joins: semi- & anti-join
## semi-join 
# fügt keine daten der zweiten tabelle hinzu, 
# sondern gibt die reihen zurück die ein entsprechung
# in der anderen Tabelle haben
# => filtert lediglich daten von der ersten tabelle
# => häufig mit nrow() kombiniert
semi_join(artists, bands, by = c("first","last"))
semi_join(artists, bands, by = c("first","last")) %>% nrow()
## anti-join
# Gegenteil von semi-join
# Gibt daten der ersten Tabelle zurück, die 
# keine entsprechung in der zweiten Tabelle haben
anti_join(artists, bands, by = c("first","last")) 
anti_join(artists, bands, by = c("first","last")) %>% nrow()

### Mengen Operationen
set.seed(0); df1 <- sample_n(mtcars,10); df2 <- sample_n(mtcars,10)
union(df1,df2)
intersect(df1,df2)
setdiff(df1,df2)
### Mengevergleich gleich (unabhänig von ordnung)
setequal(df1,rev(df1))
# + gleiche Ordnung
identical(df1,rev(df1))

#### Binding
# rbind() = bind_rows() = unten reihen anfügen
# cbind() = bind_cols() = seitlich spalten anfügen
# => cbind() probleme, wenn andere ordnung (kein id)
# => bind_rows funktionen schneller, und flexibler
#   + .id Argument, welches angibt WOHER die Reihe stammt
# => .id GUT FÜR VISUALISIERUNG MITTELS GGPLOT
set.seed(0); df1 <- sample_n(mtcars,10); df2 <- sample_n(mtcars,10)
bind_rows(Erster=df1, Zweiter=df2,.id='Datensatz')


#### data_frame
# as_data_frame analog zu as.data.frame
# evaluiert lazy und in reihenfolge => kann innerhalb referenzieren
# macht weniger automatisch: 
# z.B. NICHT Zeilennamen hinzufügen oder NICHT strings nach factors konvertieren
data_frame(
  numbers: 1:5,
  square: numbers^2)


#### Purr & reduce
# um dplyr funktionen effizient anzuwenden - z.b. mit reduce
library(purrr)
# Join von vielen Tabellen (anstatt immer wieder left_join)
list(bands, artists, artists) %>%
  reduce(left_join, by = c("first", "last"))

#### DATENBANKEN 
# src_mysql / src_postgres => ermöglicht dplyr(!) funktionen auf datenbank
# => Konvertiert dplyr Operationen nach SQL!!!
# !!!! führt befehle wie join / filter DIREKT auf DB aus
# dplyr generiert SQL-Befehle für die Operationen (z.B. group_by)
my_db <- src_mysql(dbname = "dplyr", 
                   host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                   port = 3306, 
                   user = "student",
                   password = "datacamp")
src_tbls(my_db)                       # alle Tabellen Anzeigen
nycflights <- tbl(my_db, "dplyr")     # REFERENZ auf tabelle
glimpse(nycflights)                   # zeigt nur den puffer an (17 reihen)
local_df <- collect(nycflights)       # Holt die Daten nach lokal
local_df
left_join(nycflights,nycflights,)     # wird auf server ausgeführt

nycflights %>%
  group_by(carrier) %>%
  summarise(n_flights = n(), avg_delay = mean(arr_delay)) %>%
  arrange(avg_delay)
