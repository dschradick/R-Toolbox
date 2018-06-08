########## LINEARE REGRESSION - DIAGNOSTIK
library(tidyverse)
library(dplyr)
library(modelr)
library(broom)
library(car)
library(ggfortify)

mtcars$car_name <- rownames(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)


#### Model
f <- mpg ~ disp + hp + drat + wt + cyl + gear + carb
f <- "mpg ~ disp + I(disp^2) + wt*hp + carb + gear" 
#f <- "mpg ~ log(wt)"
model <- lm(formula = f, data=mtcars)
summary(model)
model_aug <- augment(model)
model %>% tidy()
model %>% glance()

fit1 <- lm(mpg ~ wt + am + qsec, mtcars)
fit2 <- lm(mpg ~ wt + am + qsec + wt:am, mtcars)
fit3 <- lm(mpg ~ wt + am + qsec + wt:am + wt:qsec, mtcars)
fit4 <- lm(mpg ~ wt + am + qsec + wt:am + wt:qsec + am:qsec, mtcars)
anova(fit1, fit2, fit3, fit4)


## Residuen vs fitted (+ actual vs residuals)
ggplot(model_aug, aes(.fitted, .resid, label=.rownames)) + 
  geom_text(nudge_y = .3, size=2) +
  geom_point() + 
  stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") + 
  xlab("Fitted values") + ylab("Residuals") + 
  ggtitle("Residual vs Fitted Plot") +
  labs(subtitle = paste("Formula:", f), caption = paste("Adj. R^2 = ",round(glance(model)$adj.r.squared,3)))


## Einfache Diagnostik-Plots
layout(matrix(c(1,2,3,4),2,2))
plot(model)


## Diagnostic Plots mit ggplot => ggfortify
autoplot(model)
autoplot(model, which = 1:6, ncol = 3, label.size = 3)
autoplot(model, data = mtcars, label.size = 3, colour = 'cyl')



#### Residuen
glimpse(mtcars)
## Beobachtungen mit größten Residuals
# => identifizieren & ggf. getrennt behandeln
model_aug %>%
  arrange(desc(.resid)) %>%
  head() # Toyota Corolla
mtcars %>%
  filter(car_name == "Toyota Corolla") 




## QQ-Plot
ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid)) + 
  geom_point(na.rm = TRUE) + 
  geom_abline(aes(qqline(.stdresid))) + 
  xlab("Theoretical Quantiles") + ylab("Standardized Residuals") + ggtitle("Normal Q-Q") 


## Scale vs Location
ggplot(model, aes(.fitted, sqrt(abs(.stdresid)))) + 
  geom_point(na.rm=TRUE) + 
  stat_smooth(method="loess", na.rm = TRUE) + 
  xlab("Fitted Value") +ylab(expression(sqrt("|Standardized residuals|"))) + ggtitle("Scale-Location")


## Frequency Plot
ggplot(model_aug, aes(.resid)) + 
  geom_freqpoly(binwidth = 0.5)



#### Outliers

## Ranking bzgl Leverage
model_aug %>%
  arrange(desc(.hat)) %>%
  dplyr::select(mpg,disp,.fitted,.resid,.hat) %>%
  head() 

## Ranking bzgl Influence
model_aug %>%
  arrange(desc(.cooksd)) %>%
  ddplyr::select(mpg,disp,.fitted,.resid,.cooksd) %>%
  head()

## Cooks distance der einzelnen Beobachtungen
ggplot(model, aes(seq_along(.cooksd), .cooksd)) + 
  geom_bar(stat="identity", position="identity") + 
  xlab("Obs. Number") + ylab("Cook's distance") + ggtitle("Cook's distance")

## Residual vs Leverage
ggplot(model_aug, aes(.hat, .std.resid)) + 
  geom_point(aes(size=.cooksd), na.rm=TRUE) + 
  stat_smooth(method="loess", na.rm=TRUE) +
  xlab("Leverage") + ylab("Standardized Residuals") + ggtitle("Residual vs Leverage Plot") +
  scale_size_continuous("Cook's Distance", range=c(1,5)) + theme(legend.position="bottom")

## Cook's distance vs Leverage hii/(1-hii)
ggplot(model, aes(.hat, .cooksd)) + 
  geom_point(na.rm=TRUE) +
  stat_smooth(method="loess", na.rm=TRUE) + 
  xlab("Leverage hii")+ylab("Cook's Distance") + ggtitle("Cook's dist vs Leverage hii/(1-hii)") + 
  geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")



#### Hilfsfunktion für Residuen
show_residuals <- function(df, x, model){
  df_with_resids <- df %>% add_residuals(model)
  ggplot(df_with_resids, aes_string(x,'resid')) + 
    geom_ref_line(h = 0) +
    geom_point() + 
    geom_smooth(se = F) +
    ggtitle(paste0("Adjusted R^2: ", model 
                   %>% glance() 
                   %>% select(adj.r.squared) 
                   %>% round(2)))
}
mod <- lm(mpg ~ . - hp - disp  ,mtcars)
mod %>% glance()
show_residuals(mtcars,'mpg', )
