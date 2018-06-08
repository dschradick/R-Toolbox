########## STRING HANDLING
# Alle Funktionen in stringr fangen mit str_  an 
# und nehmen Vektor(!) von Strings als erstes Argument
# => gut mit dplyr zu kombinieren
#
# Geht aber auch Wert als erstes Argument
library(stringr)

#### Diverses
# Zum filtern 
mtcars <- mtcars %>% rownames_to_column(var = 'carname') 
mtcars %>% filter(str_detect(carname,'Cor'))
mtcars %>% filter(str_detect(carname,'^Toyota')) 
mtcars %>% filter(str_detect(carname,'^Toyota')) %>% count('carname')
# säubern
str_remove_all("Daniel",'a')

## Daten
x <- c("Apfel", "Banane", "Birne")

#### Base Package
tolower(x)
tolower(x[1])
toupper(x)

#### Basics
## Länge
str_length(x)

## Trim
str_trim("   Hallo ")

## Padding
str_pad(c("aa", "bbbb", "ccccc"), width = 9, side = "left", pad = "0")

## Substring
str_sub(x, 1, 2) # [1] "Ap" "Ba" "Bi"

## Einfache matchen
str_detect(x, "an")  # [1] FALSE  TRUE FALSE

## Ersetzen 
str_replace(x, "[aeiou]", "?")

## Splitten
str_split(c("a,b", "c,d,e"), ",")

## Kombination mit dplyr
data(sentences)
sentences %>%
  head(5) %>% 
  str_split(" ", simplify = TRUE) 

#### Weiteres
## Zählen von Vorkommen
str_count(x, "[aeiou]") # Anzahl der vokale => [1] 1 3 2

## Teilmenge des Vektors mit Elementen die pattern matchen
# => extrahiert die Elemente die matchen
str_subset(x, "[a]") # [1] Banane => die anderen haben kein kleines a

## Postion des Match
str_locate(x, "[aeiou]") # mit start und end

## Extraktion des Text des Matches
str_extract(x, "[aeiou]")

## Extrahieren des Inhalt der Klammern
str_match(x, "(.)[aeiou](.)")

