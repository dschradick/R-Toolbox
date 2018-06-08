########## BASICS VON R
## HILFE
args(mean)                         # Argumente anzeigen
vignette('datatable-intro',        # Längere Dokumentation
         package='data.table')
data(package = 'forecast')$result[, "Item"] # Datasets  

## ARBEITSVERZEICHNIS
path <- getwd()
setwd(path)

## PACKAGES
search()                  # zeigt pakete in der searchlist an
install.packages("ggvis") # lädt package aus CRAN repository (Comprehensive R Archive Network)
library("ggvis")          # lädt library und fügt zur searchlist hinzu
require("ggvis")          # lädt library mit überprüfung

# Package für Funktion finden
library(sos)
findFn("corrplot")

# wenn spread aufspannt
x %>% group_by(1) %>% select(-1) %>% spread(NAME_CONTRACT_TYPE,n)


## VARIABLEN
x <- 14
x
(x <- 14) # gibt x gleichzeitig aus

# Variablen-Kommentar
x <- 423
comment(x) <- "Kommentar zur Variable"
str(x)

## AUSGABE
print(x)
print(paste("x hat den Wert:",x))
message(paste("x hat den Wert:",x)) # in Rot



## TABELLE GRAFISCH
View(head(mtcars)) # => dann loslösen!!
# Rauszoomen: apfel+-
# Reinzoomen: apfel+=

## MATHEMATISCHE OPERATIONEN
TRUE == T
2 + 5 
2 ^ 5      # Potenz
10 %% 2    # Modulo
## Boolean
TRUE & TRUE
TRUE | !FALSE
## Bolean Bei Vektoren # Elementweise
c(TRUE,TRUE) & c(TRUE,TRUE) 
!c(TRUE,TRUE) 
c(TRUE,TRUE) && c(TRUE,FALSE) # Dopplelt => betrachtet nur erstes Element


## DATENTYPEN
my_numeric <- 42.5
my_character <- "some text"
my_logical <- TRUE
vector_1 <- c(1, 10, 49,40,20) # vector/array
names(vector_1) <- c('First','Second','Third','Fourth','Fifth')

# Typüberprüfung
class(my_logical)   # Typ ausgeben
class(2)
typeof(2)
is.vector(c(1,2,3)) # Typüberprüfung
l <- as.list(c(c))  # Typkonvertierung
unlist(l)
(x <- as.factor(c('a','b','c')))
unclass(x)

# Matrix Indexing => auch in data.frame
A=matrix(1:16,4,4)
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[1,]
A[-c(1,3),]
A[,-c(1)] # -c('Flower')
subset(iris, select=-c(Flower))

# Which vs boolean index
big <- which(mtcars$cyl > 4)
mtcars[big, ]            # =...
mtcars[mtcars$cyl > 4,]

# with & by
with(mtcars,mean(cyl))
by(mtcars,mtcars$cyl,function(x) c(nrow(x),mean(mtcars$hp)))

## VEKTOREN
vector_1['Third']
vector_1[c('Third','Fourth')]   # vector_1['Third':'Fourth'] geht nicht
vector_1[3]                     # index fängt bei 1 (NICHT 0) an!!
vector_1[c(2,3)] # 2 und 3
vector_1[2:3] # 2 bis 3  (Grenzen inklusive!!!! - python inclusive:exlusive
# BINARY MASK - Vergleiche & Auswahl
x <- vector_2 > 10 # => liefert TRUE/FALSE vector
# => kann als binary mask verwendet werden oder für subselection
vector_2[x] # => enthält nur die elemente > 10


# Vektor Operationen
x <- c(1, 10, 49,40,30)
which.max(x)               # index vom größten Element
rev(x)                     # umdrehen
y <- x + x                 # append
sum(x)
mean(x)
mean(x,na.rm=TRUE)         # mean mit drop nan values
cut(x, breaks = 4)         # Segementierung => für Diskretisierung!!
cut(x, breaks = 3, labels = c("eins","zwei","drei"))


## RANGES, etc
1:10                   # Sequenz generieren
seq(0,10,by=2)         
rep(c(1,2,3),times=2)  # Wiederholen
rep(c(1,2,3),each=2)


## FACTORS = Kategorische Variable = qualitative Variable
# Factor-Level = verschiedenen Kategorien
# Nominal categorical variable = keine Ordnung (z.B. Tierarten)
# Ordinal categorical variable = mit Ordnung ("gering","mittel","hoch")
# Nominal
gender_vector <- c("M", "F", "F", "M", "M")
factor_gender_vector <- factor(gender_vector,order = TRUE, levels=c('F','M')) # levels gibt ordnung an
levels(factor_gender_vector) <- c("Female","Male")
factor_gender_vector
summary(gender_vector) # summary von vector
summary(factor_gender_vector) # spezielles summary für kategorische Variable
# Ordered
speed_vector <- c("fast", "slow", "slow", "fast", "insane")
factor_speed_vector <- factor(speed_vector,ordered = TRUE,levels = c("slow", "fast",'insane'))
summary(factor_speed_vector)
factor_speed_vector[2] < speed_vector[1]


## DATA.FRAME
str(mtcars)     # Informationen über Datenset
summary(mtcars) # analog zu describe()

# data.frame erzeugen
names <- c("Daniel", "Joana", "Justine")
df <- data.frame(
        name = names,
        size = c(1,2,3))

# Wert bekommen
mtcars[1,3]     # erhält Datenstruktur von input
mtcars[[1,3]]   # liefert einfachste Datenstruktur für Output

# [[]] Benutzt in Subsetting
var <- 'cyl' # => x$var funktioniert nicht - entspricht mtcars[['var']]
mtcars[[var]]
 
# Neue Spalte
mtcars$cyl2 <- mtcars$cyl * 2

# Spalten auswählen
str(mtcars[,1])               # Erste Spalte 
str(mtcars[,-1])              # Alle ausser erste Spalte (NICHT letzte!!!)
str(mtcars[,c(2,3)])          # Spalte 2 und 3
str(mtcars[,-c(2,3)])         # alle ausser Spalte 2 und 3
str(mtcars[,c('hp','wt')])    # Spalten hp und wt 
#str(mtcars[,-c('hp','wt')])  # GEHT NICHT sondern...
str(mtcars[ ,!(colnames(mtcars) %in% c("hp", "wt"))]) 
str(select(mtcars, -hp,-wt))  # mit dplyr


# Subsetting
head(mtcars)
mtcars[1,3]          # Erste Reihe, dritte Spalte
mtcars[,3]           # Dritte Spalte
mtcars[1:2,3:4]      # Auswahlrahmen (Grenzen inklusive)
mtcars[1:10,"cyl"]   # Spaltennamen verwenden
# Aternativ: $-Notation
mtcars$cyl[1:10] # $-Notation => GLEICH vorige Zeile
# Binary Mask
mask = mtcars$cyl == 6
mtcars[mask,] # => Reiheh mit bei den Mask = TRUE
# BESSER!!!! SUBSET
mtcars_select <- subset(mtcars, cyl == 6) # Alternartive schreibweise
common_cyl <- filter(mtcars, cyl %in% c(4,6))


# Auswahl mittels attachtem data.frame
# mtcars[cyl < 6,] geht nicht direkt, alternativ:
mtcars[mtcars$cyl < 6,]    # oder attachen...
attach(mtcars)             # fügt data.frame zum suchpfad hinzu
mtcars[cyl < 6,]           # ermöglicht kurzschreibweise
detach(mtcars)

# Subsetting und Assignment
df <- mtcars
df$cyl[df$cyl < 8] <- 0
df$cyl

# Sortieren
order(mtcars$cyl,decreasing=TRUE) # => gibt indizes gemäß sortierung zurück
mtcars[order(mtcars$cyl),]

## which
# which, which.max, which.min
x <- c(1,NA,2,NA,3)
which(!is.na(x))                # Indizes in denen Bedingung wahr
mtcars[which.min(mtcars$cyl),]  # Reihe mit Minimum / Maximum


## STRINGS
x <- "Hallo"; y <- "Welt"; z = "H a l l o"
paste(x, y, sep = ' ')    # Join von mehreren Strings
paste(x, collapse = ' ')  # Join innerhalb eines Strings
grep("allo", x)           # Regex pattern finden => siehe regex
gsub("allo", "amm", x)    # Ersezen von pattern 
toupper(x)
tolower(x)
nchar(x)


## Lists
# Können jede Art von Datentyp aufnehmen
my_list <- list(numbers = 1:10, 
                my_matrix = matrix(1:9, ncol = 3), 
                df = mtcars[1:10,])
str(my_list)                             # Struktur angucken
my_list$df[1]                            # mit $ elemente zugreifen 
my_list <- c(my_list, numbers_2 = 11:20) # Daten hinzufügen


# For-Schleife
primes <- c(2, 3, 5, 7, 11, 13)
for (p in primes) {
  print(p)
}

## FUNKTIONEN
# return() nicht notwendig => letzter evaluierter Ausdruck
# Funktionen sind objekte
add <- function(x, y = 4) {
  return(x + y) 
}
ratio(x = 3,4)
(function(x){x+1})(2) # anonyme Funktion (wie lambda Funktion)

# ... = ellipsis = Funktion kann mehrere benannte / unbenannte Argumente nehmen
addPercent <- function(x, mult = 100, ...){
  percent <- round(x * mult, ...)
  paste(percent, "%", sep = '')
}
addPercent(10, digits = 2)


### APPLY
# kann meist statt for-loop verwendet werden

## lapplay
# wendet funktion auf jedes element an - 
# liefert immer(!) Liste zurück
numbers <- c(1,2,3,4,5)
result <- lapply(numbers,function(x){x+1})
result
str(result)
result.vector <- unlist(result) # erzeugt aus list ein vector
str(result.vector)
str(lapply(numbers,function(x,y){x+y},y=3)) # bei funktionen mit mehreren args mit übergeben

## sapply = simplified apply - 
# output: vector => versucht zu simplifizeren (zu array zu machen)
# automatisch unlist + benennung (problem wenn outputs nicht gleiche länge haben => keine simplification)
# -> liefert einen benannten vector (mit gleichen datentypen)
# => bessere formatierung
result <- sapply(numbers,function(x){x+1})
class(result)

##  vapply
# output format kann explizit spezfiziert werden 
# => sichere alternative zu sapply
names <- c('daniel','joana','corbinian','justine')
sapply(names,nchar)
vapply(names,nchar,numeric(1))

## mapply - Apply mit mehreren Argumenten
vars1<-c(1,2,3) ; vars2<-c(10,20,30)
mult_one<-function(var1,var2){var1*var2}
mapply(mult_one,vars1,vars2) # 10 40 90
  
  
## EINFACHE MATHEMATISCHE FUNKTIONEN
sum(c(1,2,3))
round(2.1)
abs(-3)
sd(c(1,2,3))

## Elemente des Fits anzeigen
mtcars$hp
fit <- lm(hp ~ .,  data=mtcars)
names(fit)


## REGEX
# grepl: liefert binary index 
# grep:  liefert array indizes
# sub vs gsub => sub ersetzt nur erstes vorkommen
emails <- c("danny@mmojoe.com","joana@hotmail.com","corbi@gmx.net")
grepl("mmo",emails)
hits <- grep("mmo",emails)
emails[hits]                             # auswahl
sub("@.*\\.com$","@google.com",emails)   # ersetzung

## DATES
today <- Sys.Date()         # Datum
now <- Sys.time()           # Datum und Uhrzeit
today - 3                   # Tage subrahieren
str <- "30/January/2006"
date <- as.Date(str, format = "%d/%B/%Y") # Konvertieren
birth <- as.POSIXct("1879-03-14 14:37:23")
death <- as.POSIXct("1955-04-18 03:47:12")
diff <- death - birth
library(lubridate)
ymd(today) + months(3) + days(3)  # Erweiterte Datumsberechnung


## MATRIZEN
matrix(1:9, byrow = TRUE, nrow = 3)
matrix_data = c(vector_1,vector_1,vector_1)
m <- matrix(matrix_data, byrow=TRUE, nrow=3)
colnames(m) <- c('col1','col2,','col3','col4','col5')
rownames(m) <- c('row1','row2','row3')
m
# Auswahl
m[1,] # erste Reihe
m[,1] # erste Spalte
m[,"col1"] # erste Spalte
for (i in colnames(m)){
  print(m[,i])
}
# Operationen
m * 10           # elementweise multiplikation 
rowSums(m)
# Anfügen von weiterer Matrix / Vektor
m_with_row_sum <- cbind(m,rowSums(m)) 
m_with_col_sum <- rbind(m,colSums(m)) 

# Plotting
require("ggplot2")
qplot(mtcars$wt, mtcars$hp)
