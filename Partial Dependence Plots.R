library(pdp)           
library(vip)           
library(tidyverse)        # KEIN tidyverse => problem mit partial
library(randomForest)  


#### Vorbereitungen
partial <- pdp::partial
# => sonst fehler wenn tidyverse importiert, weil partial anders gebunden


#### Modelle Fitten
# Random Forest & Lineare Regression
set.seed(0)  
mtcars_rf <- randomForest(mpg ~ ., data = mtcars, importance = TRUE)
mtcars_lr <- lm(mpg ~ ., data = mtcars)


## Variable importance plot 
vip(mtcars_rf, bar = FALSE, horizontal = FALSE, size = 1.5)  # Figure 1
randomForest::varImpPlot(mtcars_rf)


## Einfacher PDP
partialPlot(mtcars_rf, pred.data = mtcars, x.var = "hp") 


#### Univariate PDPs
#p1 <- partial(mtcars_rf, pred.var = "hp", plot = TRUE, rug = TRUE)
p1 <- partial(mtcars_rf, pred.var = "hp", plot = TRUE, plot.engine = "ggplot2",ice = F)
p2 <- partial(mtcars_lr, pred.var = "hp", plot = TRUE, plot.engine = "ggplot2", ice=F)
grid.arrange(p1, p2, ncol = 2)  


#### Multivariate PDPs
pd <- partial(mtcars_rf, pred.var = c("hp", "disp"), plot.engine = "ggplot2")
pdp1 <- plotPartial(pd)
rwb <- colorRampPalette(c("red", "white", "blue"))
pdp2 <- plotPartial(pd, contour = TRUE, col.regions = rwb)
pdp3 <- plotPartial(pd, levelplot = FALSE, zlab = "cyl", colorkey = TRUE, 
                    screen = list(z = -20, x = -60))

grid.arrange(pdp1, pdp2, pdp3, ncol = 3)


#### Fehlinterpretation bei zu wenig Werten verhindern
# => verwenden von chull
p1 <- partial(mtcars_rf, pred.var = c("hp", "disp"), plot = TRUE, chull = TRUE)
p2 <- partial(mtcars_rf, pred.var = c("hp", "disp"), plot = TRUE, chull = TRUE,
              palette = "magma")
grid.arrange(p1, p2, nrow = 1)
