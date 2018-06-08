########## EDA 
library(tidyverse)
library(gapminder)

path <- file.path("~","Documents/R/data","comics.csv")
comics = read.csv(path,stringsAsFactors=TRUE)
path <- file.path("~","Document/R/data","life_exp_raw.csv")
options(scipen = 999, digits = 3) 

## Schablone für Vorgehen:
# 0. Datensatz allgemein: head, dimensionen, summary, null-werte, duplikate
# 1. Univariat
# 2. Bivariat
# 2a.  Scattermatrix & Korrelationsmatrix als Ausgangpunkt 
# 2b.  Korrelation mit Zielvariable als Barplot
# 2c1.  Numerisch vs numerisch: Scatter
# 2c2.  Kategorisch vs numerisch: side-by-side Boxplots / Violin
# 3.Aus 2a und 2b die hösten Korrelation anzeigen und nacheinander plotten
# 4. Multivariat
#    - Welche Variablen untersuchen?
#     => Stepwise Regression für intressierte / Zielvariable als Ausgangspunkt (zeigt, welche var info hinzufügt)
#    - Zielvariable als Kategorisch (transformieren) und dann als colour 
#          => Verlauf der Farbe in beide Achsenrichtungen betrachten
#    - Verifkation & Einfluss bestimmen durch 
#     Effekt: lm(zielvariable ~ var1 + var2) vs lm(zielvariable ~ var1) => unterschied in r^2
#     Einfluss: Slope von var1 & var2 betrachten     
# 4a. Welche mit der Zielvariable korrelierten Variablen gibt es -> multivariat
# 4b. Überlagerte Histogramme / Density-Plots / Faceted Scatterplot
# 4c. Scatterplots mit Farbe/Größe
# MERKE:
# - wenn bei multiple regression eine dritte variable information hinzufügt => dann höher als bei zwei
# - wenn bivariater Plot sehr ungleich, dann zuerst log auf die Variable
# - diskretisieren von kontinuierlichen Variablen zur kategorischen - 
#    => überlagtertes histogram möglich
#    => aufgeteilte Boxplots möglich
#    => häufig im multivariaten Scatterplot besser auseinanderzuhalten


#### Daten Vorbereiten
# Level droppen => Level mit count == 0, werden entfernt
str(comics)
comics <- comics %>%
  filter(align != "Reformed Criminals") %>%
  droplevels()
head(comics)

#### KATEGORISCHE DATEN
# Kategorische Werte einer Variable anzeigen
levels(comics$hair)

## (Univarite) Verteilung textuell anzeigen 
comics$id <- factor(comics$id,levels = c("No Dual","Public", "Secret","Unknown")) # Select & Reorder 
table(comics$id) # Textuell

## Historgram
ggplot(comics,aes(x=id)) + geom_bar() # Barchart
# Bar- vs Pie-Chart (kann man schlecht die relativen Größen der Stücke vergleichen)

## Grouping by level - Faceting, Stacking, Side-by-Side
table(comics$id,comics$align) # textuell
ggplot(comics,aes(x=id)) + # Grafisch
  geom_bar() + 
  facet_grid(~align)  # facet_grid(vertical,horizontal)
# vs Stacking (-> jede Facette eine Bar)
ggplot(comics, aes(x = align, fill = id)) + 
  geom_bar(position='fill') 
## Side-by-Side Barcharts
ggplot(comics, aes(x = align, fill = id)) + 
  geom_bar(position = 'dodge') 
# => beide Richtugngen
ggplot(comics, aes(x = id, fill = align)) + 
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90))


#### Anzahl vs Proportionen
# Textuell-Count (Contingency table)
table(comics$id,comics$gender)
# Textuell-Proportion
tab <- table(comics$id, comics$gender)
prop.table(tab)     # Gesamt-Verhältnis 
prop.table(tab, 1)  # Verhältnis in Reihen
prop.table(tab, 2)  # Verhältnis in Spalten
# Grafisch
ggplot(comics, 
       aes(x = align, fill = gender)) + 
       geom_bar() # Count
ggplot(comics, aes(x = align, fill = gender)) + 
       geom_bar(position = "fill") # Normalisiert = Proportion



#### NUMERISCHE DATEN
path <- file.path("~","R/data","cars.csv")
cars = read.csv(path,stringsAsFactors=FALSE)
str(cars)

## Univariat
# Dotplot
ggplot(cars, aes(x = weight)) + geom_dotplot(dotsize = 0.4)
# Histogram
ggplot(cars, aes(x = weight)) + geom_histogram(binwidth=100)  
# Density
ggplot(cars, aes(x = weight)) + geom_density(bw=100) # bw = bandwidth
# Boxplot (Problem bei mehren Hügeln, x für kategorische Variable)
ggplot(cars, aes(x = 1, y = weight)) + geom_boxplot() + coord_flip()  


##### MULTIVARIAT
# Scatterplot: durch Farben, shapes, size alpha usw für weitere Dimensionen
ggplot(cars, aes(x = as.factor(horsepwr), 
                 y = dealer_cost,
                 color=factor(ncyl),
                 alpha=eng_size)) + geom_point() 
# => oder mit mehren Graphen überlagern, nebeneinander durch...
# Kombinierte Boxplots - sehr gut zum vergleich von levels!
common_cyl <- filter(cars, ncyl %in% c(4,6,8))
ggplot(common_cyl, aes(x = as.factor(ncyl), y = city_mpg)) + geom_boxplot()
# Faceted Histogram (kategorischer Variable => conditional distribution)
ggplot(cars, aes(x = hwy_mpg)) + geom_histogram() + facet_wrap(~pickup)
# Überlagte Density Plots
ggplot(common_cyl, aes(x = city_mpg, fill = as.factor(ncyl))) + geom_density(alpha = .3)
# Überlagerte Histogramme => geom_histogram(alpha=.7, position="identity")
# Scatterplot & Diskretisierung
ggplot(iris,aes(x=Petal.Length,y=Petal.Width)) + geom_point() # Scatter und nun Diskretisierung
ggplot(iris, aes(x = cut(Petal.Length, breaks = 5), y = Petal.Width)) + geom_boxplot() 



## Bedingte Wahrscheinlichkeit
common_cyl %>%
  mutate(big_engine = eng_size > 2)  %>%
  ggplot(aes(x = as.factor(big_engine), y = as.factor(suv))) +
    geom_bar(position = "fill") 

# > 3 Variablen
common_cyl %>%
  ggplot(aes(x = hwy_mpg)) +
  geom_histogram() +
  facet_grid(ncyl ~ suv,labeller = label_both) +
  ggtitle("")

######## 4 Charakteristiken von Verteilungen #######
# 1. Center, 
# 2. Spread (Variability) 
# 3. Shape
# 4. Outliers

### MEASURES OF CENTER
# = mean, median und mode
# Gruppiertstr(mtcars)
mtcars %>%
  group_by(cyl) %>%  # Alernativ: slice(10:20) %>% 
  summarize(mean(mpg),median(mpg))
  
### MEASURES OF VARIABILITY / SPREAD
sd(mtcars$mpg)            # Standard deviation
var(mtcars$mpg)           # Variance
IQR(mtcars$mpg)           # (Breite der) IQR
diff(range(mtcars$mpg))   # Range 
# Center + Variability durch
summary(mtcars$mpg)
# Gruppiert
gap2007 <- filter(gapminder, year == 2007)
gap2007 %>%
  group_by(continent) %>%
  summarize(sd(lifeExp),
            IQR(lifeExp),
            n())
# Gruppiert als density plots
gap2007 %>%
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.3)


## SHAPE = MODALITY & SKEW
# MODALITY = Anzahl der Hügel in der Verteilung
# Anuzahl der Hügel = Uniform, Unimodal, Bimodal, Multimodal, 
# SKEW = Schiefe
# Left,Right-skewed oder symmetrisch
# Seite = Wo ist der Tail 
# BEI EXTREMEN SKEW - Variable mit log(x) transformieren.
# Bsp:
gap2007 %>% 
  ggplot(aes(x = pop)) + geom_density()
# Log-Transform!!
# log(0) = -Inf in R, deshalb + .01) 
gap2007 <- gap2007 %>%
  mutate(log_pop = log(pop + .01))
gap2007 %>%
  ggplot(aes(x = log_pop)) + geom_density()


## OUTLIERS
# Sollten seperat untersucht werden
# Plotting ohne Outliers, welcher sich auf den Body der Verteilung konzentriert
gap_asia <- gap2007 %>%
  filter(continent == 'Asia') %>%
  mutate(is_outlier = lifeExp < 50)
p1 <- gap_asia %>%
  ggplot(aes(x = lifeExp)) + geom_density()
# vs
p2 <- gap_asia %>%
  filter(!is_outlier) %>%
  ggplot(aes(x = lifeExp)) + geom_density()

## Anzeigen nebeneinander
multiplot(p1, p2, cols=1)

## ENTFERNUNG VON OUTLIERN durch Pipe
cars %>% 
  filter(msrp < 25000) %>%
  ggplot(aes(x=horsepwr)) +
  geom_histogram() +
  xlim(c(90, 550)) +
  ggtitle('HP price < 25000')




multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
