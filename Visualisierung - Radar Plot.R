library(pheatmap)
library(tidyverse)
library(ggradar)
library(scales)

path <- file.path("~/Documents/Data","","wineQualityReds.csv")
reds <- read.csv(path)
reds <- reds %>% mutate(quality.level = ifelse(reds$quality < 5, "low", ifelse(reds$quality < 7, "average", "high")))
reds$quality.level <- factor(reds$quality.level,levels=c("low", "average", "high"), ordered=TRUE)
reds <- reds[2:14]


reds_radar <- reds %>%
  mutate_if(is.numeric, funs(rescale)) %>%
  group_by(quality.level) %>%
  summarise_all(mean) 

ggradar(reds_radar,axis.label.size = 5, values.radar = c("","",""),
        group.line.width = 0.5, 
        group.point.size = 2,
        grid.line.width = 0.2,
        legend.text.size = 8,) 
