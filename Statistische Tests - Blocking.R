########## STATISTISCHE TESTS - ANOVA & BLOCKING
#### One-Way ANOVA 
df <- data.frame(cbind(a=x,b=x,c=x))
df <- stack(df)
colnames(df) <- c('value','group')

oneway.test(value ~ group, df)       # schnellversion
fit <- aov(value ~ group, data = df) # ausführlicher
summary(fit)
layout(matrix(c(1,2,3,4),2,2)) ; plot(fit) 
summary(lm(value ~ group, df))       # als regression  

## Post hoc Vergleich mit Tukey
TukeyHSD(fit)

#### Blocking einer Variable
# Test von 3 neuen menu items in 6 ausgewählten Filialen 
# => Effekt der Filiale kontrollieren durch blocking

## Daten
data = ("
        Menue1 Menue2 Menue3
        31    27    24 
        31    28    31 
        45    29    46 
        21    18    48 
        42    36    46 
        32    17    40")
data = read.table(textConnection(data),header=TRUE)
r = c(t(as.matrix(data)))
f = c("Menue1", "Menue2", "Menue3")   # treatment levels 
k = 3                                 # anzahl treatment levels
n = 6                                 # anzahl blocks
# gl erzeugt Faktoren gemäß Muster ihrere Level
(tm = gl(k, 1, n*k, factor(f)))         # Vektor von treatment factors, welche jedem Element in r entsprechen
(blk = gl(n, k, k*n))                   # blocking factor 
(df <- data.frame(r,tm,blk)) 
str(df) 
## ANOVA ohne & mit Blocking
av = aov(r ~ tm, df) ; summary(av)       # ohne Blocking
av = aov(r ~ tm + blk, df) ; summary(av) # mit Blocking => geringere Varianz
TukeyHSD(av) # => zwischen 2 und 3 unterschied


# Grafischer Vergleich der Mittelwerte 
# 1. Interaction Plots
# 2. Means mit Error Bars

# Interaction Plot
library(gplots)
attach(mtcars)
gears <- factor(gears)
cyl <- factor(cyl)
interaction.plot(df$tm, df$blk, df$r, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="Menues", 
                 ylab="Käufe", 
                 main="Interaction Plot")

# Means mit Error Bars
library(gplots)
attach(mtcars)
cyl <- factor(cyl)
plotmeans(r ~ tm,data = df,xlab="Menüs",
          ylab="Käufe", main="Mean Plot\nwith 95% CI")

