########### VISUALISIEREN - MOSASIC + GGFORMULA
library(mosaic)
library(ggformula)

### Einfache Beispiele
gf_point(mpg~disp, data= mtcars)
gf_props(~cyl, data=mtcars)
gf_hex(carat ~ price, data=diamonds)


#### Kategorische Daten tabellarisch
tally(mtcars$cyl)
tally(~cyl, data = mtcars)
tally(~cyl, data = mtcars, format = "perc")


#### Statistiken
mean(~cyl | am ,  data=mtcars)
sd(~cyl | am ,  data=mtcars)

confint(t.test(~mpg, data = mtcars))
confint(t.test(mpg ~ am, data = mtcars))


#### Plots
gf_qq(~mpg, data=mtcars)
gf_bar(~as.factor(cyl), data=mtcars)
gf_bar(~as.factor(cyl) | am, data=mtcars)

gf_point(mpg ~ hp, data=mtcars)
gf_dens(~mpg, data=mtcars)
gf_dens(~mpg | am, data=mtcars, color=~as.factor(cyl))
gf_histogram(~mpg,data=mtcars)
gf_histogram(~mpg | am, data=mtcars, fill=~as.factor(cyl))
gf_boxplot(mpg ~ as.factor(am) | as.factor(cyl), data = mtcars)
