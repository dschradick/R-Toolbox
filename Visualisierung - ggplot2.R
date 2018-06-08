########## GGPLOT2

# Packages: 
# https://www.youtube.com/watch?v=mH-Me-yYkzs
# ggally: eda & regression model plots
# ggstatsplot: normale plots mit statistik test info versehen
# gghighlight: hervorheben von Linien, bei mehreren Linien
# ggtheme - fertige themes & anpassen für branding
# ggridges: conditional denstities übereinander
# ggrepel: textlabel werden auseinander gezogen => mit pfeilen 



# Grammar of Graphics
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(quantreg)
library(Hmisc)
require(scales)
library(GGally)

#### Diverses
## Jupyter Plot Größe
# => besonders bei horizontalen boxplots 
options(repr.plot.width=8, repr.plot.height=4)
## Beschriftungen
mtcars %>% 
  ggplot(aes(mpg,hp)) + 
  geom_point() +
  labs(title='Dies ist der Titel',
     subtitle='Untertitel',
     legend="Überschrift Legende", 
     caption="Dies ist die Caption",
     x='x', y='y') + 
  scale_x_continuous(trans = 'log2') + 
  
#### Elemente der Grammatik
# => sind jeweils jeweils Layers
# 1. Layer: Data = dataset 
# 2. Layer: Aesthetics(aes) = besagt auf welche Scale die Daten gemapped werden sollen (z.B. sepal.length -> X, sepal.width -> > Y)
# 3. Layer: Geometries = wie sieht der Plot aus (z.B. Scatterplot)
# Optional:
# 4. Layer: Facets: wie soll der Plot gesplitted werden (z.B. für jeden Species) 
# 5. Layer: Statistics: um mehrere verschiedene Parameter hinzuzufügen
# 6. Layer: Coordinates: spezifiziert die Dimensionen des Plots
# 7. Layer: Themes: Ink, welche nichts mit den Daten zu tun hat

#### Beispiel
ggplot(iris,                                          # Data
       aes(x=Sepal.Length, y = Sepal.Width)) +        # Aesthetics
       geom_jitter(alpha = 0.6) +                     # Geometries
       facet_grid(. ~Species) +                       # Facets
       stat_smooth(method='lm', se = F, col='red') +  # Statistics
       geom_smooth(method='loess') +
       scale_y_continuous("Sepal Width (cm)",         # Coordinates
                          limit = c(2,5),
                          expand = c(0,0)) +
       scale_x_continuous("Sepal Length (cm)",
                           limit = c(4,8),
                           expand = c(0,0)) ;
       #coord_equal()
       #theme(panel.background = element_blank(),      # Theme
       #      plot.background = element_blank()) 


#### DATA
# Tidy Data: ggplots benötigt tidy data für beste Darstellung

# Nicht: (Fehler an keine Legende erkennbar)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  geom_point(aes(x = Petal.Length, y = Petal.Width), col = "red")

# SONDERN: Tidy Data erzeugen und dann plotten
iris$Flower <- 1:nrow(iris) ; head(iris)

iris.wide = gather(iris,key,value,-Species,-Flower)   ; head(iris.wide)
iris.tidy = separate(iris.wide,key, c("Part", "Measure"), "\\.") ; head(iris.tidy)
iris.wide = spread(iris.tidy,Measure, value) ; head(iris.wide)
#iris.wide2 = dcast(iris.tidy,  formula = Flower + Part + Measure ~ Species) ; head(iris.wide2)
#iris.wide2 = spread(iris.tidy, Species, value) ; head(iris.wide2) # y u not work?!?!

# Dann korrekt:
ggplot(iris.tidy, aes(x = Length, y = Width, col = Part)) +
  geom_point()

# Oder dann facet grid:
ggplot(iris.wide, aes(x = Length, y = Width, color = Part)) +
  geom_jitter() +
  facet_grid(~ Species, scales='fixed') + # free_y
  scale_color_brewer(type='qual')


##### Aesthetics
# "y (mpg) wird durch x (wt) beschrieben" 
# beschreiben das mapping von Variablen auf...
# Achsen, colour (Umrandung), fill (Füllfarbe) - geht nicht bei allen, size (durchmesser), alpha (0 = transparent) - bei overplotting, 
# linetype (Line dash pattern), labels (text auf plot oder achse), shape
# Werden sie in geom_(point) getan => dann sind es attribute welche gesetzt werden (und nicht aesthetics welche gemapped werden)
ggplot(mtcars, aes(x = wt, y = mpg, size = (hp/wt), color = mpg, fill = cyl)) +  # Verhältnis durch size=(hp/wt)
  geom_point(shape = 23, alpha=0.6)  # 21,23 hat fill und (color)outline
ggplot(mtcars, aes(x = wt, y = mpg, alpha=cyl)) +  # alpha für variable :-)
  geom_point()
ggplot(mtcars, aes(x = wt, y = mpg)) +  # Text Datenpunkte
  geom_text(label=rownames(mtcars))

## Positions
# = wie sollen überlappende elemente in einem layer
# identity = default - genau an den punkt der vorgegeben
# Scatter (jitter = noise hinzu)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(position="jitter")

cyl.am <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) 
# Barcharts
# => weight argument, wenn daten bereits aggregiert & somit nur ein Wert
#    alternativ: stat = 'identity' und y Variable angeben
cyl.am + geom_bar(position="stack")  # einfach aufeinander=> count
cyl.am + geom_bar(position="fill")   # bis oben hinfüllen => ratio 
cyl.am + geom_bar(position="dodge")  # nebeneinander

mtcars %>%            
  count(cyl, sort=T) %>% 
  mutate(cyl = fct_reorder(factor(cyl), n)) %>% 
  ggplot(aes(cyl,weight=n)) + 
  geom_bar() + 
  coord_flip()

## Brewer
# scale_color_brewer, scale_fill_brewer
# type = "div" = divergent, "seq" = sequential, "qual" = categorical 
# oder palette - eg. 'Blues'
scale_fill_brewer(type='qual')

## Scales
# Beschriftungen, Limits, Grid-Linien
val = c("#E41A1C", "#377EB8") ; lab = c("Manual", "Automatic")
cyl.am +
  geom_bar(position = "dodge") +
  scale_x_discrete("Cylinders") + # kategorische Variable
  scale_y_continuous("Number") +  # kontinuierliche Variable
  scale_fill_manual("Transmission", values = val,labels = lab) 
# weitere: limits = c(2, 8), breaks = seq(2, 8, 3) - grid-unterbrechungen, expand = c(0, 0)) 
ggplot(mtcars, aes(x = mpg, y = 0)) +  geom_point() + geom_jitter() + scale_y_continuous(limits=c(-2,2))

## Overplotting
# verwenden z.B. von transparenz, size, jitter, leeren shapes(z.b. 1),  andere achsen
# ===> parameter sollten immer optimiert werden
# Mehrfarbige Farbverläufe vermeiden
# Scatter plot: carat (x), price (y), clarity (color)
ggplot(diamonds,aes(x=carat,y=price,col=clarity)) + geom_point()
ggplot(diamonds,aes(x=carat,y=price,col=clarity)) + geom_point(alpha=0.5)  # alpha
ggplot(diamonds,aes(x=clarity,y=carat,col=price)) + geom_point(alpha=0.5) # Achsen ändern
ggplot(diamonds,aes(x=clarity,y=carat,col=price)) + geom_jitter(alpha=0.5,shape=1) # jitter + shape
# geom_jitter(width=0.1)

### GEOMETRIES
# Parameter als aesthetic mappings oder als Attribut
# Untere Layers vererben data und aesthetics
# aes in geom() => erlaubt die aesthetics für jedes Layer einzeln zu kontrollieren
# data kann auch im Layer gesetzt werden


## Scatter plots

# geom_points() 
# Benötigt: x,y
# Optional: alpha, colour, fill, shape, size 
ggplot(diamonds,aes(x=clarity,y=carat,col=price)) + geom_jitter(alpha=0.5,shape=1) # jitter + shape


ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point() +  # vererbt data und ebs
  #geom_point(data = iris.summary, shape = 15, size = 5)  +# ander Daten, aber vererbtes aes 
  geom_vline(data = iris.summary, aes(xintercept = Septal.Length))


## Histogram 
# ..density.. = proportionale Frequenz des Bins zum gesamten Datenset
# ..count.. = Anzahl (wenn kein y angegeben ist automatisch ausgewählt)
ggplot(mtcars, aes(mpg)) +
  geom_histogram(stat = "bin", aes(y = ..density..), binwidth = 1, fill = "#377EB8")

# Histogram mit (kategorischem) Fill
ggplot(mtcars, aes(x=mpg,fill=as.factor(cyl))) + geom_histogram()

# Position
ggplot(mtcars, aes(x = cyl, fill = factor(am))) + geom_bar() # stack
ggplot(mtcars, aes(x = cyl, fill = am)) + geom_bar(position = "stack") # count
ggplot(mtcars, aes(x = cyl, fill = am)) + geom_bar(position = "fill")  # proportion
ggplot(mtcars, aes(x = cyl, fill = am)) + geom_bar(position = "dodge") # nebeneinnader
posn_d <- position_dodge(width = 0.2) # teilweise überlappende bars
ggplot(mtcars, aes(x = cyl, fill = am)) + 
  geom_bar(position = posn_d,alpha = 0.6) # => geht nicht
# non-id vs id => stack bei übelappen
ggplot(mtcars, aes(mpg,fill=factor(cyl))) + geom_histogram(binwidth = 1) # identiy bei overlap von verschiedenen kategorien
ggplot(mtcars, aes(mpg,fill=factor(cyl))) + geom_histogram(binwidth = 1,position="identity") # identiy bei overlap von verschiedenen kategorien
# Alternativ mit Frequency Polygon
ggplot(mtcars, aes(mpg)) + #,col=factor(cyl))) + 
  geom_histogram(binwidth = 1, position="identity") +
  geom_freqpoly(binwidth=1)

# Historgram für Ordinals  => FARBVERLAUF!!!
ggplot(mtcars, aes(x = cyl, fill = factor(gear))) + geom_bar(position = "fill") + scale_fill_brewer()
blue_range <- colorRampPalette(brewer.pal(6, "Blues") ) # Reds
ggplot(mtcars, aes(x = cyl, fill = factor(gear))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = blue_range(3))

## Line Plots
# Insbesondere für zeitliche Verläufe geeignet
# Verschiedene Lines am besten durch Farben zu kennzeichnen
# geom_area mit position = "fill" für zeitlichen Verlauf von Proportionen
# bei überlappendem geom_area => geom_ribbon mit alpha (gut um unsicherheit zu plotten)

# Einfachen zeitlichen Verlauf plotten
ggplot(economics, aes(x = date, y = unemploy)) + geom_line()
# Plotten eines Verhältnis über die Zeit
ggplot(economics, aes(x = date, y = unemploy/pop)) + geom_line()
# Verlauf mit mit markierten Bereichen 
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_rect(data = recess,
            aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf),
            inherit.aes = FALSE, fill = "red", alpha = 0.2) + geom_line()
# Verlauf von mehreren Zeitreihen
fish.tidy <- gather(fish.species, Species, Capture, -Year)
ggplot(fish.tidy, aes(x = Year, y = Capture,col=Species)) + geom_line()


##### qplot
# Quick and Dirty plotting mit ggplot
# verwendet automatisch die richtigen geoms
# vs baseplot: verwendet grammar of graphics => flexiber - z.B. weiter informationen durch col,size...

## Vergleich base base vs ggpot vs qplot
plot(mpg ~ wt, data = mtcars)      # Base Plot - Formel Notation
with(mtcars, plot(wt, mpg))        # Base Plot - x,y Notation
## ggplot
ggplot(mtcars, aes(wt, mpg)) +    # ggplot
   geom_point() 
qplot(wt, mpg, data = mtcars)      # qplot

# Grammar of graphics einbauen
qplot(factor(cyl),data=mtcars)                          # Automatische Auswahl des geoms
qplot(factor(cyl),factor(vs),geom='jitter',data=mtcars) # oder explizit geom
qplot(wt, mpg, data = mtcars, color = hp)               # grammar of graphics verwenden



#### Statistics-Layer
# Alle Funktionen beginnen mit stat_
# => könnend direkt oder indirekt durch geom aufgerufen werden
# z.B. z.B. geom_histogramm verwendet stat_bin = Anzahl der Elemente in den jeweiligen Gruppe bestimmen
# geom_bar verwendet stat_bin als default stat
# geom_smooth verwendet stat_smooth

ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + geom_smooth(span = 0.8) # loess-smooth = moving avarage
# LOESS smoothing ist nicht nicht parametrische Form der Regression, welche ein gewichteten sliding window Durschnitt um den besten Fit zu berechnen n
# => Window size kann durch span gesetzt werden
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() +  geom_smooth(method='lm', fullrange = TRUE) # OLS mit confidence Interval
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() +  geom_smooth(method='lm', se=FALSE)        # ohne CI-Band
ggplot(mtcars, aes(x = wt, y = mpg)) + stat_smooth(method='lm',se=FALSE)                         # ohne Datenpunkte

# Linien für Gruppe und für alle zusammen
myColors <- c(brewer.pal(3, "Dark2"), "black")
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = F, span = 0.75) +
  stat_smooth(method = "loess", 
              aes(group = 1, col="All"), 
              se = F, span = 0.7) +
  scale_color_manual("Cylinders", values = myColors)

# Color Palette für Gruppe
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  stat_smooth(method = "lm", se = F) + scale_color_brewer() 

# Color Gradient für Gruppe
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl, group = factor(cyl))) +
  stat_smooth(method = "lm", se = F, alpha = 0.6, size = 2) + scale_color_gradientn(colors = brewer.pal(9,"YlOrRd")) 


# Quantil Regression
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl, group = factor(cyl))) +
  stat_quantile(alpha = 0.6, size = 2, quantiles = 0.5) +
  scale_color_gradientn(colors = brewer.pal(9,"YlOrRd"))


# Varianz & Errorbars Plotten 
mtcars$cyl <- as.factor(mtcars$cyl) ; mtcars$am <- as.factor(mtcars$am)
posn.d <- position_dodge(0.1) ; 
posn.jd <- position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)
posn.j <- position_jitter(0.2)
wt.cyl.am <- ggplot(mtcars, aes(x = cyl, y = wt, col = am, fill = am,group = am))
wt.cyl.am + geom_point(position = posn.jd, alpha = 0.6)


### Automatische Statisiken mit anzeigen
# Wrappers um Funktionen von Hmisc
# mean_sdl => smean.sdl in Hmisc
# mult=multiplier des standard error

## Mittelwert & Standardabweichung
wt.cyl.am + stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), position = posn.d)
# Mittelwert & 95% Konfidenz-Intervall
wt.cyl.am +  stat_summary(fun.data = mean_cl_normal, position = posn.d)
# Mean & SD - mit T-tipped error bars 
wt.cyl.am +
  stat_summary(geom = "point", fun.y = mean, position = posn.d) +
  stat_summary(geom = "errorbar", fun.data = mean_sdl, position = posn.d, fun.args = list(mult = 1), width = 0.1)


### Coordingates Layer
# Funktionen beginnen mit coord_
# Zooming in mit: scale_x_continuous(limit = ), xlim() - gehen Punte möglicherweise verlohren und Gruppen ändern sich hierdurch
# bei coord_cartesian gehen keine Punkte verloren

p <- ggplot(mtcars, aes(x = wt, y = hp, col = am)) + geom_point() + geom_smooth()
# Punkte werden möglicherweise vorher rausgefiltert
p + scale_x_continuous(limits = c(3,6), expand = c(0, 0))
# Beser: keine Punkte gehen verloren
 p+ coord_cartesian(xlim = c(3,6))
 
# Seiteverhältnis setzen 
base.plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + geom_jitter() + geom_smooth(method = "lm", se = F)
base.plot
 # 1:1 Seitenverhältnis setzen
base.plot + coord_equal() # oder mit
base.plot + coord_fixed()

# Pie Chart
wide.bar <- ggplot(mtcars, aes(x = 1, fill = cyl)) + geom_bar(width = 1)
wide.bar + coord_polar(theta = "y")
 
  
### FACETS-LAYER
# Daten in mehrere Diagramme aufsplitten, welche das gleiche Koordinatensystem benutzen
# => andere Möglichkeit kategorische Variablen darzustellen
#  facet_gid(rows ~ colums)

p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p + facet_grid(am ~ .)   # reihen durch am
p + facet_grid(. ~ cyl)  # spalten durch cyl
p + facet_grid(am ~ cyl) #  beides

### THEMES-LAYER
# Visuelle Elemente, welche nicht Teil der Daten sind 
# z.B. (Position von) Titel, Axenbeschriftungen, Grid, etc
# 3 Typen: text, line, rectangle
# verwendet Vererbung


# Plot 1: change the plot background color to myPink:
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p + theme(plot.background = element_rect(fill = myPink))

# Plot 2: adjust the border to be a black line of size 3
z + 
  theme(plot.background = element_rect(fill = myPink, color = "black", size = 3))

# Plot 3: set panel.background, legend.key, legend.background and strip.background to element_blank()
uniform_panels <- theme(panel.background = element_blank(), 
                        legend.key = element_blank(), 
                        legend.background=element_blank(), 
                        strip.background = element_blank())
z + 
  theme(plot.background = element_rect(fill = myPink, color = "black", size = 3)) +
  uniform_panels



#### Beispiele
ggplot(diamonds, aes(x = carat, y = price)) + geom_smooth() 
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() # + geom_smooth() # => häufige kombi
ggplot(diamonds, aes(x = carat, y = price, color=clarity)) + geom_smooth(se=TRUE)  # se = standard error
ggplot(diamonds, aes(x = carat, y = price, color=clarity)) + geom_point(alpha = 0.4)
# es können später andere layer durch + layer zum ggplot hinzugefügt werden

str(mtcars)
# vs
ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point() #
# vs (als kategorische variable)
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_point()

# color (analog zu hue) und size 
ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) + geom_point()
ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) + geom_point()



##### Base Package vs ggplot2 - Scatterplot mit Regressionsgeraden
# Plot zeichnet einfach nur ein Image, bei zweitem Layer einfach rübergezeichnet (nicht neugezeichner), keine automatische Legende
# ggplot erzeugt ein plot obj
### Mit Base Package
## Scatter Plot
mtcars$cyl <- as.factor(mtcars$cyl) # macht farben zu 1,2,3
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)
## Regressions-gerade
carModel <- lm(mpg ~ wt, data = mtcars)
abline(carModel, lty = 2) # lty = linetype
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)
lapply(mtcars$cyl, function(x) {abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x)})
legend(x = 5, y = 33, legend = levels(mtcars$cyl), col = 1:3, pch = 1, bty = "n")
# => extrem aufwendig => Besser mit ggplot


## Scatterplot mit Regressionsgerade für einzelne Farbe/Gruppen und das ganzes Dateset
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() + # Scatterplot
  geom_smooth(method = "lm", se = FALSE) + # Regressionsgeraden der Gruppen
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, linetype = 2) # Regressionsgerade des Datensatzes

