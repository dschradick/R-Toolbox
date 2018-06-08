########## STATISTISCHE TESTS - MITTELWERT 
#### One-sample t-test
library(ggpubr)

## Daten erzeugen
set.seed(0)
data <- data.frame(
  name = paste0(rep("Person_", 10), 1:10),
  weight = round(rnorm(10, 20, 2), 1))

## Daten betrachten
data
summary(data$weight)

ggboxplot(data$weight, 
          ylab = "Weight", xlab = FALSE,
          ggtheme = theme_minimal())

## Bedingungen
# Sample Größe - weniger 30 observations?
# Daten normal? 
shapiro.test(data$weight) # => p-value = 0.6993 => nicht signifikant verschieden von normal
ggqqplot(data$weight, ylab = "Gewicht",
         ggtheme = theme_minimal())

## Test durchführen
mean(data$weight)
res <- t.test(data$weight, mu = 25)

res 
# => signifikant => gleichheit kann ausgeschlossen werden 
# einzelne Werte anzeigen
res$p.value ; res$statistic ; res$estimate ; res$conf.int

# Wenn zeigen, dass mittlerer Wert kleiner als 25
t.test(data$weight, mu = 25, alternative = "less")

## Alternativ: Nicht-parametrisch 
# One-Sample Wilcoxon Signed Rank Test
# => wenn normalität nicht angenommen werden kann 
# => median statt mean
wilcox.test(data$weight, mu = 25)


#### Unpaired 2-sample t-test
## Daten erzeugen
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 
data <- data.frame( 
  group = rep(c("Woman", "Man"), each = 9),
  weight = c(women_weight,  men_weight)
)

## Bedingungen
# Unabhängig => ja, sind verschieden
# Normal:
with(data, shapiro.test(weight[group == "Man"])) # p = 0.1
with(data, shapiro.test(weight[group == "Woman"]))  # p = 0.6
# Gleiche Varianz
var.test(weight ~ group, data = data)

## Test durchführen
res <- t.test(women_weight, men_weight, var.equal = TRUE) # oder als data.frame..
res <- t.test(weight ~ group, data = my_data, var.equal = TRUE) # => signifikant => Gleichheit kann ausgeschlossen werden  
# Gewicht von Männer < Frauen
t.test(weight ~ group, data = my_data, var.equal = TRUE, alternative = "less")
res$p.value ; res$statistic ; res$estimate ; res$conf.int


## Alternativ: Nicht-parameterisch (median)
# exact = F, weil test annimmt dass response kontinuierlich (warnung)
wilcox.test(weight ~ group, data = my_data,exact = FALSE)




#### Paired 2-sample t-test
## Daten generieren
before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
my_data <- data.frame( 
  group = rep(c("before", "after"), each = 10),
  weight = c(before,  after))

## Visualisieren: Vorher => Nachher
library(PairedData)
pd <- paired(before, after)
plot(pd, type = "profile") + theme_bw()

## Bedingungen
# Paired? Ja => vorher und nachher bei DENSELBEN probanden
# Sample gross? Nein => Normalitätstest
d <- with(my_data, weight[group == "before"] - weight[group == "after"])
shapiro.test(d) # => p-value = 0.6141

## Test durchführen
res <- t.test(before, after, paired = TRUE) # oder als data.frame...
res <- t.test(weight ~ group, data = my_data, paired = TRUE)
res$p.value ; res$statistic ; res$estimate ; res$conf.int

## Alternativ: nicht-parametrisch (median)
wilcox.test(weight ~ group, data = my_data, paired = TRUE)

