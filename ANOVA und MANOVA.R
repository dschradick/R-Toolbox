########## ANOVA UND MANOVA
#### One-way ANOVA
# Erweiterung von t-test auf mehr als zwei Gruppen
# Daten eingeteilt in mehrere Gruppen basierend auf Gruppierungsvariable-Variable 
# => Gruppierungsvariablen ist Faktor (deshalb auch 1-Faktor ANOVA)
# Vergleich 3 Gruppen A,B,C 
# 1. Berechne Varianz innerhalb der sample (s^2_within)
# 2. Berechne  Varianz zwischen den sample means, wie folgt:
# 2a. Berechne die Mittelwert jeder Gruppe
# 2b. Berechne die Varianz zwischen den sample means (s^2_between)
# 3. Erzeuge F-Statistik als das Verhältnis s^2_between / s^2_within
# ==> ratio < 1 => keine signifikanter Unterschied
library(dplyr)
library(ggpubr)

data <- PlantGrowth

#### Daten betrachten
## Textuell
group_by(data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

## Boxplots
ggboxplot(data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

## Meanplots
library("ggpubr")
ggline(data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")



#### Test durchführen
res.aov <- aov(weight ~ group, data = data)
summary(res.aov)

### Bedingungen
# Daten normal verteilt 
plot(res.aov, 2)
# Homogenität der Varianz innerhalb der Gruppen
plot(res.aov, 1) # gekennzeichnete Punkte sind Outlier
library(car)
leveneTest(weight ~ group, data = data) # größer 0.05 => nicht signifikant verschieden

## Alternativ: mit welch-test, welcher keine Homogenität der Varianz benötigt
oneway.test(weight ~ group, data = data)


### Post-hoc - Tukey mehrfach paarweiser Vergleich 
# Welche Gruppen sind verschieden (anstatt überhaupt eine anders)
TukeyHSD(fit) # oder...

library(multcomp)
summary(glht(res.aov, linfct = mcp(group = "Tukey"))) # oder.

pairwise.t.test(data$weight, data$group,p.adjust.method = "BH")

## Alternativ: nicht-parametrisch
# Kruskal-Wallis rank sum test
kruskal.test(weight ~ group, data = data)



#### Two-way ANOVA
## Daten generien
data <- ToothGrowth
data$dose <- factor(data$dose, levels = c(0.5, 1, 2), labels = c("D0.5", "D1", "D2"))
head(data)

## Daten betrachten
table(data$supp, data$dose)
# Summary Statistiken
group_by(data, supp, dose) %>%
  summarise(
    count = n(),
    mean = mean(len, na.rm = TRUE),
    sd = sd(len, na.rm = TRUE))
# Boxplot
ggboxplot(data, x = "dose", y = "len", color = "supp",
          palette = c("#00AFBB", "#E7B800"))
# Meanplot
ggline(data, x = "dose", y = "len", color = "supp",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))
# Interaction Plot
interaction.plot(x.factor = data$dose, trace.factor = data$supp, 
                 response = data$len, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Dose", ylab="Tooth Length",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))


## Test durchführen
# Additives Modell
res.aov2 <- aov(len ~ supp + dose, data = data)
summary(res.aov2)
# Modell mit Interaktionen
res.aov3 <- aov(len ~ supp * dose, data = data) # äquivalent zu
res.aov3 <- aov(len ~ supp + dose + supp:dose, data = data)
summary(res.aov3)

### Post-hoc - Tukey mehrfach paarweiser Vergleich 
TukeyHSD(res.aov3, which = "dose") # oder
summary(glht(res.aov2, linfct = mcp(dose = "Tukey")))

## Bedingungen
# Homogenität der Varianz
plot(res.aov3, 1)
leveneTest(len ~ supp*dose, data = data)
# Normalität
plot(res.aov3, 2)


## Unbalanced Design
my_anova <- aov(len ~ supp * dose, data = data)
Anova(my_anova, type = "III") # verschiedene Möglichkeiten => III = sum-of-squares



#### MANOVA
# = multivariate analysis of variance
# Verwenden bei mehreren response Variablen
# Beispiel: Experiment mit zwei treatments und interessiert an Gewicht und Größe
data <- iris

### Test durchführen
# Existiert signifikanter Unterschied in Sepal und Petal Länge zwischen den Spezien?
res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
summary(res.man) # ja

## Und welche unterscheiden sich?
summary.aov(res.man)

### Interpretation
# Globaler multivariater Test signifikant => effekt von treatment signifikant
# Danach: Welche Variable beinflusst das Treatment - Größe oder Gewicht
# Also welche abhängige Variable hat zu globalem Effekt beitgetragen 
# => entscheiden durch One-Way ANOVA

### Bedingungen
# Abhängige Variablen sollten innerhalb der Gruppen normalverteilt sein
# => mshapiro.test() 
# Homogenität bzgl aller predictors
# Lineariät