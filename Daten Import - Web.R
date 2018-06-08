########## IMPORT VON WEBDATEN

#### Dateien herunterladen
## Normal
url_rdata <- "https://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/swimming_pools.csv"
download.file(url_rdata, destfile = "swimming_pools.csv")

## Mit httr
# => für komplexere requests (z.B. authentification)
library(httr)
url <- "http://www.example.com/"
resp <- GET(url)
resp
content(resp, as = "raw")
content(resp, as = "text")


#### Einlesen von CSV / TSV
library(readr)
url_csv   <- "https://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/swimming_pools.csv"
url_delim <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/potatoes.txt"
# => geht mit http und https
pools <- read.csv(url_csv)  # normale read.csv
pools <- read_csv(url_csv)  # über die readr 
potatoes <- read_tsv(url_delim) 


#### Excel
library(readxl)
library(gdata)  
# => gdata kann direkt aus internet laden (readxl nicht)
url_xls <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/latitude.xls"
excel_gdata <- read.xls(url_xls)
# zuerst Datei herunterladen - muss man bei read_excel
download.file(url_xls, destfile = "local_latitude.xls")
excel_readxl <- read_excel("local_latitude.xls") # 


#### APIs & JSON
# Load the jsonlite package
library(jsonlite)

## JSON
json <- '{"a": [1, 2, 3], "b": [4, 5, 6]}'
# Von JSON importortieren
data <- fromJSON(json)  
# Nach JSON exportieren
toJSON(mtcars) 
# Pretty & mini format
toJSON(mtcars, pretty = T) 
minify(toJSON(mtcars))

## Von API laden
url_sw <- "http://www.omdbapi.com/?apikey=ff21610b&i=tt0076759&r=json"
sw <- fromJSON(url_sw)
sw$Title
sw$Year > sw$Year


