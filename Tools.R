########## TOOLS


## Options
options(width=120)    # für großen monitor - standard ist 80
options(scipen=999)   # normale darstellung
options(scipen=0)     # exponential darstellung

## Datasets in Package
d <- data(package = "ggplot2")
d$results[, "Item"]
txhousing

## Größe in MB von Variable
print(object.size(df_postgres), units='Mb')

## Alle Variablen löschen
rm(list=ls())

## Datei als String lesen
file_content <- readChar(fileName, file.info(fileName)$size)

## Zeit messen
system.time({
  x <- 1:100000
  for (i in seq_along(x))  x[i] <- x[i]+1
})

## Schrittweise Zeit messen
library(tictoc)
tic(); print("Hello"); toc();
tic("From Start"); Sys.sleep(2); tic("From middle"); Sys.sleep(3); toc() ; toc()
?tic

## Numerische Spalten
nums <- sapply(x, is.numeric)
df %>% select_if(is.numeric)

## Mapping von Values
plyr::mapvalues(mtcars$am, from=c(0,1), to=c('manual','automatic'))

## Spaltennamen Komma getrennt => schnell vektor für auswahl machen
cat(colnames(train),sep=",") 

## data.frame sortieren
mtcars <- mtcars[order(mtcars$cyl,mtcars$mpg,decreasing = T),]

## Assignment direkt ausgeben lassen
(x <- 1)

## case_when: multiple ifelse zuweisung mit dplyr 
mtcars %>% 
  mutate(cg = case_when(carb <= 2 ~ "low",
                        carb == 3 ~ "medium",
                        carb > 3 ~ "high")) 

## Mehrere Packages installieren
load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("lavaan","semPlot","semTools","nonnest2","htmlTable"); load(packages)

## Plot von Missing data
library(Amelia) 
missmap(mtcars)


## Plot von Missing data
library(ggplot2)
plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}
library(MASS)
plot_Missing(Boston)


## hübschen + Schriftart setzen
+ theme_bw() + theme(text = element_text(family = "serif", size = 11))

## Scales beschriften
scale_x_continuous(breaks = 1:15) +
scale_y_continuous(label = scales::percent) # oder z.B. scales::dollar

## Line-Plot mit Punkten
geom_line() + geom_point() 

## Farben manuel setzen
bp + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))  # Box plot
sp + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) # Scatter plot

## Sample Daten erzeugen
n <- rnorm(n = 100, mean = 100, sd = 15)
p <- rpois(n = 100, lambda = 10) 
b <- rbinom(n = 10, size = 100, p=.2) 
