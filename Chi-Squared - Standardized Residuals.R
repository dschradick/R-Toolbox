library(tidyverse)
library(gmodels)

# Gutes Bsp für Abhänigigkeit
CrossTable(mpg$class, mpg$drv, 
           sresid = T, expected = T, 
           prop.r = F, prop.c = F,
           fisher = T,                # für Konfidenzintervalle
           format = "SPSS",)           # SPSS notwendig für sresid

  # Cell Contents
  # |-------------------------|
  # |                   Count |
  # |         Expected Values |
  # | Chi-square contribution |
  # |             Row Percent |
  # |          Column Percent |
  # |           Total Percent |
  # |            Std Residual |   => achten auf |stdresid| > 1.96 
  # |-------------------------|  


chisq.test(mpg$class, mpg$drv)
