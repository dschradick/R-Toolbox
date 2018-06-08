########## DATEN IMPORT - LOKAL

#### Pfad setzen
setwd("~/Documents/Data/")
# Oder
path <- file.path("~","~/Documents/Data/","swimming_pools.csv")


#### tidyverse/readr: read_csv
# read_delim hauptfunktion => andere sind wrapper
# basiert auf parse_... col_....
# parse_number("60 cars") => 60          => für column: col_number()
# parse_number("60 cars") => "60 cars"  
# parse_datetime  => col_date(format = "%d %B %Y")
# col_skip() - um Spalte beim einlesen zu ignorieren
# cols_only() - nur spezifierte spalten einlesen
library(tidyverse)
data <- read_csv("swimming_pools.csv",
                 col_types = cols(Latitude = col_number()))
data <- read_csv("swimming_pools.csv",
                 col_types = cols_only(Latitude = col_number()))
glimpse(data)

## tibble
tbl_df(iris)                # nach tibble konvetieren
as.data.frame(tbl_df(iris)) # zurück nach data.frame

# => read_csv ist Wrapper von read_delim
## read_delim
# Optionen:
# skip, n_max für chunk-reading
# col_names = c("Name", "Address", "Latitude","Longitude"), col_types = 'cccd')
# c = char, d = double, i = integer, l = logical, _ = skip
data <- read_delim(path,delim=",",
                   col_names = c("Name", "Address", "Latitude","Longitude"), 
                   col_types = 'cccc')


#### RData
# Läd mehrere gespeicherte Datensets
# => Variablennamen werden automatisch zugewiesen
# save(list = ls(all = TRUE), file= "all.rda") ; rm(oldvariables)
local({
  load("~/Documents/Data/all_polls.RData")
  ls()
})


#### RDS & 
loan_data <- readRDS("~/Documents/R/data/loan_data_ch1.rds")
# => muss Variable zugewiesen werden


#### CSV - utils: read.csv
data <- read.csv('swimming_pools.csv',stringsAsFactors = FALSE)         # normale csv 
# => read.csv ist Wrapper von read.table
data <- read.table('swimming_pools.csv', header=TRUE, sep=",",stringsAsFactors = FALSE
                    , dec='.'
                    , col.names = c("Name", "Address", "Latitude","Longitude")
                    , colClasses = c("factor", "character","character", "numeric")) 
data
# => read.table ist hauptfunktion und kann ziemlich alle Flatfiles lesen (.csv,.delim nur wrapper)


#### tidyverse/readr: read_csv
# schneller und benutzt tibbles (mehr funktionen) statt data.frame
# hier read_delim hauptfunktion => andere sind wrapper
library(readr)
library(dplyr)
tbl_df(iris)                # nach tibble konvetieren
as.data.frame(tbl_df(iris)) # zurück nach data.frame
data <- read_csv(file,
                 col_types = cols(Latitude = col_number()))
glimpse(data)
# => read_csv ist Wrapper von read_delim
## read_delim
# Optionen:
# skip, n_max für chunk-reading
# col_names = c("Name", "Address", "Latitude","Longitude"), col_types = 'cccd')
# c = char, d = double, i = integer, l = logical, _ = skip
data <- read_delim(path,delim=",",
                   col_names = c("Name", "Address", "Latitude","Longitude"), 
                   col_types = 'cccc')

#### data.table
# schnell; insbesondere für größe datensätze
library(data.table) 
data = fread(path, select= c('Name','Address'))
data

#### Excel
install.packages('readxl')
library(readxl)
path <- file.path("~","R/data","urbanpop.xlsx")
excel_sheets(path)
pop_1 <- read_excel(path, sheet = 1)
my_workbook <- lapply(excel_sheets(path), read_excel, path=path)

