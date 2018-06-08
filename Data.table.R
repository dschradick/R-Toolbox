########## DATA.TABLE
# !!!! Kann wie data.frame benutzt werden => dann aber meist genauso LANGSAM 
# => vector scanning (linear) vs binary-seach O(log n)
# DF: DT[x == "a" & y == "b",]           => data.frame vector scanning
# DT: setkey(DT,x,y); DT[.("a","b")]     => data.table binary search
# Bemerkung: kann mit dplyr kombiniert werden, führt aber meist zu Geschwindigkeitsverlust
library(data.table)

DT <- data.table(V1=c(1L,2L), # L = explizit integer
                 V2=LETTERS[1:3],
                 V3=round(rnorm(4),4),
                 V4=1:12)

#### DT[i, j, by]
# Nimm DT, subset rows mit i, dann berechne j gruppiert nach by
#
#   R:      i                 j        by
# SQL:  where   select | update  group by
# 
# Besser: (insbesondere) bei Chaining von rechts nach links lesen

#### i: Subsetting von Reihen
DT
DT[2:4]
DT[.N-1] # .N = Number of rows => vorletzte Reihe
DT[V2=='A'] 
DT[V2 %in% c('A','C')] 


#### j: Spaltenauswahl (+ ggf Operation)
## .() = Rückgabe als data.table
## Auswahl
DT[,V2]
DT[,.(V2,V3)]
# Operationen
DT[,sum(V1)]                                # einfache Operation
DT[,.(sum(V1),mean(V3))]                    # data.table zurückgeben
DT[,.(Aggregate=sum(V1), mean.V3=mean(V3))] # wie mutate
DT[,.(V1, mean.V3=mean(V3))]
DT[,.(print(V2), plot(V3), NULL)]           # V2 ausgeben, V3 plotten

## by: group by  
DT[, .(V4.sum=sum(V4)), by=V1]          # einfache Gruppierung
DT[, .(V4.sum=sum(V4)), by=.(V1,V2)]    # mehrer Grouping Variabalen
DT[,.N,by=V1]                           # Anzahl von Reihen in jeder Gruppe

#### Hinzufügen / Update von Spalten 
## in j mit := 
## [] hinter ausdruck gibt Erbnis (zusätzlich) aus
DT[,V5:=round(exp(V1),2)]                      # hinzufügen
DT[,V5:=NULL][]                                # löschen
DT[,V1:=round(exp(V1),2)]                      # updaten
DT[,c("V1","V2"):=list(round(exp(V1),2),LETTERS[4:6])]   # mehrere Updates gleichzeitig
DT[,':='(V1=round(exp(V1),2), V2=LETTERS[4:6])]          # andere Schreibweise
DT[,':='(V1=round(exp(V1),2), V2=LETTERS[4:6])][]        # => gleich ausgeben
#Cols.chosen=c("A","B")     # löschen von mehreren
#DT[,Cols.Chosen:=NULL]     # !! löscht DIE SPALTE Cols.Chosen!!
#DT[,(Cols.Chosen):=NULL]   # löscht die in Cols.Chosen enthaltenen Spalte => Unterschied

#### Indizes und Keys
# Re-orded auch gleich nach Spalte(n)
# wird insbesondere für joins benötigt
setkey(DT,V2)   # V2 als Key setzen => danach...
tables()  # => zeigt keys an
DT[c("A","C")]  # direkte abfrage auf V2
DT[c("A","D"),nomatch=0]              # alle Zeilen in denen key-column A oder D
DT[c("A","D")]
DT[c("A","C"),sum(V4)]                # Dann andere spalte aufsummieren
DT[c("A","C"), sum(V4), by=.EACHI]    # gruppiert für jeden Schlüsselwert
#!!! 
setkey(DT,V1,V2)           # Multi-Schlüssel 
DT[.(2,"C")]               # = join...
DT[data.table(2,"C"),]     # = kurzgeschrieben...
DT[J(2,"C")]



#### Subsetting
# .SD ist das Subset / Gruppe, für jeden eindeutigen Wert der by Variable
# .SDcols nach by Element zur Spaltenauswahl vom Subset
# => häufig kombiniert mit lapply: DT[, lapply(.SD, median), by = x]
DT
DT[,print(.SD),by=V2]  
DT[,.SD[c(1,.N)],by=V2]  # Erste und letzte Reihe in den jeweiligen Gruppen von V2
DT[,.(SD.sum=lapply(.SD,sum)), by=V2] 
DT[,.(SD.sum=lapply(.SD,sum)), by=V2, .SDcols=c("V3","V4")] 

#### Chaining
# Auswahl nach der "inneren" Auswahl
DT2 <- DT[,.(V4.Sum=sum(V4)), by=V1] ; DT2
DT2[V4.Sum > 40]
DT[,.(V4.Sum=sum(V4)), by=V1][V4.Sum > 40]   # => Chaining
DT[,.(V4.Sum=sum(V4)), by=V1][order(-V1)]

#### Joining
# ON / USING wird umgesetzt durch durch setzen von keys mittels setkey
# TABLE_X[TABLE_Y] ist outer join
# TABLE_X[TABLE_Y, nomatch=0] ist inner join
DT2 <- data.table(id=c(1L,2L), Name=c('daniel','joana','corbi','justine'))
DT[,name_id:=1:3]
setkey(DT,name_id)
setkey(DT2,id)
DT2[DT,nomatch=0]
DT2[DT,allow.cartesian=TRUE]
merge(DT,DT2, all.x=F, by.x = 'name_id', by.y = 'id')

#### Setting
# Schleifenbasierte Version von :=
rows <- list(3:4,5:6) ; cols <- 1:2
for(i in seq_along(rows)) { set(DT, i=rows[[i]], j=cols[i], value=NA) }

#### weiteres
setnames(DT,"V2","Rating")                          # V2 => Rating
setnames(DT, c("V2","V3"), c("V2.rating","V3.DC"))  # V2 und V3 umbenennen
DT[.N-1]  # Vorletzte Reihe vs
DT[,.N]   # Anzahl der Reihen

##### vs. data.frame
dt <- as.data.table(mtcars) 
# Get the mean MPG by Transmission
tapply(mtcars$mpg, mtcars$am, mean)  # wird zu
dt[,mean(mpg),by=am]
# Get the mean MPG for Transmission grouped by Cylinder
aggregate(mpg~am+cyl,data=mtcars,mean)
dt[,.(avg=mean(mpg)),by=.(am,cyl)]


