########## MYSQL
# Wichtig: Besser mit dplyr als direkt
# => hält Referenz statt ganzen dataframe zu laden - siehe unten...
library(RMySQL)
library(DBI)

### Verbinden mit Mysql
connect.to.mysql <- function(){
  dbh <- dbConnect(MySQL(), 
                   dbname = "tweater", 
                   host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                   port = 3306,
                   user = "student",
                   password = "datacamp")
}

#### Verbinden 
con <- connect.to.mysql()
# dbDisconnect(con)  

#### Tabellen anzeigen
tables <- dbListTables(con)  # Tabelle anzeigen
str(tables)  

#### Tabellen lesen
## Eine Tabelle
users <- dbReadTable(con, "users") 
users
## Alle Tabellen
table_names <- dbListTables(con)
tables <- lapply(table_names, dbReadTable, conn = con)
str(tables)
tables

#### Queries
dbGetQuery(con, "SELECT * FROM comments WHERE user_id > 4")
## Chunking
res <- dbSendQuery(con, "SELECT * FROM comments WHERE user_id > 4")
dbFetch(res, n = 2)
dbFetch(res)
dbClearResult(res)

#### DPLYR 
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
local_df <- collect(nycflights)       # Holt die Daten nach local
local_df
left_join(nycflights,nycflights,)     # wird auf server ausgeführt

nycflights %>%
  group_by(carrier) %>%
  summarise(n_flights = n(), avg_delay = mean(arr_delay)) %>%
  arrange(avg_delay)
