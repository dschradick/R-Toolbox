library(GGally)

#vignette(package='GGally') # None

# Matrix von Plots
p1 <- ggplot(mtcars, aes(hp, mpg)) + geom_point() 
ggmatrix(list(p1,p1,p1,p1), nrow=2, ncol=2)
ggmatrix(list(p1,p1,p1,p1), nrow=2, ncol=2, title='Title',
         xAxisLabels = c('x1','x2'), yAxisLabels = c('y1','y2'),
         showYAxisPlotLabels = FALSE)

### ggduo !!!!
# => Pairplot mit konfigurierbaren Variablen für x & y

# MÖGLICHKEITEN => Funktionen wie ggally_smooth_loess
# continuous = 'smooth_loess'
vig_ggally("ggally_plots")
# z.B.  
# continuous = ('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank').
# combo = ('box', 'box_no_facet', 'dot', 'dot_no_facet', 'facethist', 'facetdensity', 'denstrip', 'blank')
# discrete = ('facetbar', 'ratio', 'blank')#

#continuous: when both x and y variables are continuous
#comboHorizontal: when x is continuous and y is discrete
#comboVorizontal: when x is discrete and y is continuous
#discrete: when both x and y variables are discrete
#na: when all x data and all y data is NA


mtcars$cyl <- factor(mtcars$cyl)
ggduo(
  mtcars,
  columnsX = c('cyl', 'hp', 'wt'),
  columnsY = c('mpg', 'drat'),
  mapping = ggplot2::aes(color = am, shape=vs),
  types = list(
    continuous = wrap("smooth_lm", alpha = 0.50, se=F),
    comboVertical= "box"
  )
)

# Model Diagnostik
mod <- lm(mpg ~ ., data=mtcars) 
mod <- step(mod)

# ggnostic() wrapper für ggduo()
ggnostic(mod)
ggnostic(mod, columnsY = c('.resid')) 
# von broom => c('mpg', '.fitted', '.se.fit', '.resid', '.hat', '.sigma', '.cooksd', '.std.resid') 

# Bivariate Übersichrt
ggpairs(mtcars)

# Coef Plots
mod_reg    <- lm(mpg ~ . , data=mtcars)
mod_logreg <- glm(as.numeric(am) ~ . , data=mtcars)
ggcoef(mod_reg, exclude_intercept = T, sort = "descending")
ggcoef(mod_logreg, exponentiate = T, exclude_intercept = T, sort = "descending")
# bessere version von ggcoef
ggcoef_model(mod_reg)
# => yo! :-)

## Modelle vergleichen
mod_reg    <- lm(mpg ~ . , data=mtcars)
mod_step   <- step(mod_reg)
models <- list("Full model" = mod_reg, "Simplified model" = mod_step)
ggcoef_compare(models, type = "faceted")     
     
# note # stepwise selection
mod_step <- step(mod, direction = 'both') 

ggally_points(mtcars, mapping = ggplot2::aes(x = mpg, log(carb)))

# interactions
ggduo(mtcars, aes(x = disp, y = am))


# Kontingenz-Tafeln
mtcars$am <- factor(mtcars$am)
mtcars$vs <- factor(mtcars$vs)
diamonds$price_cat <- cut(diamonds$price, 4)

ggtable(mtcars, 'vs','am', fill = 'std.resid')
# Proportions
# "prop", "row.prop" or "col.prop"
ggtable(mtcars, 'vs','am', fill = 'std.resid', cells = "prop")
ggtable(mtcars, 'vs','am', fill = 'std.resid', cells = "row.prop")
ggtable(diamonds, c('color','price'), c('cut', 'price_cat'), fill = "std.resid")

  
