########## DPLYR - ÜBERSICHT
# Referenz: https://dplyr.tidyverse.org/reference/index.html 
library(tidyverse)
library(lubridate)

## Vignetten
vignette('dplyr', package='dplyr')

vignette('window-functions', package='dplyr')
vignette('two-table', package='dplyr')
vignette('grouping', package='dplyr')
vignette('colwise', package='dplyr')
vignette('rowwise', package='dplyr')


starwars
# entfernt 2x2.5% = 5% der verteilung an den enden
mean(diamonds$price, trim=0.025)

glimpse(mtcars)
table(mtcars$cyl)

#### REIHEN MIT NAs
mtcars %>% filter_all(any_vars(is.na(.)))             # in allen spalten
mtcars %>% filter_if(is.numeric, any_vars(is.na(.)))  # in numerischen spalten
mtcars %>% filter(is.na(cyl))                         # in einer spalte 


# Non-Standard & Standard Evaluation
mtcars %>% group_by(cyl)
mtcars %>% group_by_('cyl')

#### DIVERSES
## Spaltennamen
names(mtcars) = sub(" ", "_", names(mtcars))           # Leerzeichen => _
names(mtcars) = names(mtcars) %>% tolower()            # Kleinschreibung
## Reihennamen
m <- mtcars %>% rownames_to_column(var='carname')      # Reihennamen => Spalte 
m %>% column_to_rownames(var='carname')                # Spalte => Reihennamen           
remove_rownames(mtcars)                                # Reihennamen löschen

## Diskretisieren
mtcars %>% 
  mutate(hp_bins = case_when(hp < 150 ~ 'low',
                             hp < 200 ~ 'medium',
                             TRUE ~ 'high'))

## Werte umbennen 
# Level genordnet nach auftreten beim aufruf
am <- recode_factor(mtcars$am, '1' = 'automatic', '0' = 'manual')

## Reorder von Level
levels <- c('0','1')
mtcars$am <- factor(mtcars$am, levels)
levels(mtcars$am)




#### BEOBACHTUNGEN
### Extrahieren
mtcars %>% filter(cyl > 5)
mtcars %>% filter(between(cyl,4,5))
mtcars %>% filter(cyl > 5, am == 1)                    # Mehrere
mtcars %>% filter(hp > quantile(hp,.95))               # top 5%
mtcars %>% distinct(cyl)     
mtcars %>% sample_n(10)                                # Random sample - Anzahl
mtcars %>% sample_frac(.1)                             # Random sample - Prozent
mtcars %>% slice(1:5)                                  # Auswahl nach Position 
mtcars %>% head(5)     

# distinct values
mtcars %>% distinct_at(vars(cyl, am))


### Sortieren
mtcars %>% arrange(qsec) 
mtcars %>% arrange(desc(qsec)) 


#### SPALTEN
## Auwahl
mtcars %>% select(1:3,5)                               # Position
mtcars %>% select(hp,am)                               # Name
mtcars %>% select(1,am)                                # Position + Name
mtcars %>% select(-am,-carb)                           # ALLE AUSSER
mtcars %>% select_if(is_numeric)                       # NUMERISCHE

## Hinzufügen
mtcars %>% add_column(new = 1:32)                      # Neue Spalte durch Werte
mtcars %>% mutate(mpg_per_hp = mpg / hp )              # Neue Spalte berechnen
mtcars %>% mutate(mpg_per_hp = mpg / hp )              # Neue Spalte berechnen

## Nur neue Spalte behalten
mtcars %>% transmute(mpg_per_hp = mpg / hp)            # alle anderen spalte entfernt

## Umbenennen
mtcars %>% rename(ps = hp)                             # Neue Spalte durch Werte


#### Summarizing
mtcars %>% summarise(mean_carb = mean(carb))          
mtcars %>% summarise_all(mean)

## Anzahl & Verhältnis
mtcars %>% summarise(count = n())                      # Anzahl Reihen
mtcars %>% summarise(count = n_distinct(cyl))          # Anzahl unique values
mtcars %>% summarise(prop_high_ps = sum(hp > 150))     # Anzahl mit Bed.   
mtcars %>% summarise(prop_high_ps = mean(hp > 150))    # Proportion   

## Count visualisieren
counts <- mtcars %>% group_by(cyl) %>% summarise(count = n()) %>% arrange(desc(cyl))
counts$cyl <- factor(counts$cyl, levels = counts$cyl[order(counts$count,decreasing=T)])
ggplot(counts, aes(x=as.factor(cyl), y = count)) + geom_bar(stat="identity")


### Window Funktionen
# rank() => base-r
# row_number():    equivalent to rank(ties.method = "first")
# min_rank():      equivalent to rank(ties.method = "min")
# dense_rank():    like min_rank(), but with no gaps between ranks
# percent_rank():  a number between 0 and 1 computed by rescaling min_rank to [0, 1]
# cume_dist():     a cumulative distribution function. Proportion of all values less than or equal to the current rank.
# ntile():         a rough rank, which breaks the input vector into n buckets. The size of the buckets may differ by up to one, larger buckets have lower rank.
# lag()
# lead()

mtcars %>% 
  mutate(lagged_mpg = lag(mpg),
         rnk = rank(hp, ties = 'first')) %>% 
  select(mpg,lagged_mpg,rnk) 

mtcars %>% 
  group_by(cyl) %>%
  arrange(cyl,mpg) %>%
  mutate(cumsum_mpg = cumsum(mpg)) %>%
  mutate(cummax_mpg = cummax(mpg)) %>%
  mutate(denserank_mpg = dense_rank(mpg)) %>%
  mutate(cume_dist_mpg = cume_dist(mpg)) %>%
  mutate(ntile_mpg = ntile(mpg,5)) %>%
  select(mpg,cumsum_mpg,cummax_mpg,denserank_mpg,cume_dist_mpg,ntile_mpg) %>%
  print(n=20)



## Lage 
mtcars %>% summarise(mean_hp = mean(hp))
mtcars %>% summarise(median_hp = median(hp))
## Varianz: IQR(), sd(), var()




### JOINS

## Mutating Joins
# left_join(x, y)             
#  - use all variables that appear in both tables, a natural join.
# left_join(x, y, by="x")    
#  - by = "x". Like a natural join, but uses only some of the common variables        
# left_join(x, y, by = c("x" = "a")
#  - Will match variable x in table x to variable a in table y. The variables from use will be used in the output.

## Filtering Joins
# semi_join(x, y) KEEPS all observations in x that have a match in y.
# anti_join(x, y) DROPS all observations in x that have a match in y.

## Set Operations

# intersect(x, y): return only observations in both x and y
# union(x, y):     return unique observations in x and y
# setdiff(x, y):   return observations in x, but not in y.
