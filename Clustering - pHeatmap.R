########## CLUSTERING HEATMAP
library(pheatmap)
library(tidyverse)

path <- file.path("~/Documents//Data","","wineQualityReds.csv")
reds <- read.csv(path)
reds <- reds %>% mutate(quality.level = ifelse(reds$quality < 5, "low", ifelse(reds$quality < 7, "average", "high")))
reds$quality.level <- factor(reds$quality.level,levels=c("low", "average", "high"), ordered=TRUE)
reds <- reds[,2:13]
glimpse(reds)
# Subsampling der einzelnen Klassen
# => meistens reichen offensichtlich wenige Observations
n <- 50
set.seed(0)
reds_3 <- sample_n(filter(reds,quality == 3),n)
reds_4 <- sample_n(filter(reds,quality == 4),n)
reds_5 <- sample_n(filter(reds,quality == 5),n)
reds_6 <- sample_n(filter(reds,quality == 6),n)
reds_7 <- sample_n(filter(reds,quality == 7),n)
reds_8 <- sample_n(filter(reds,quality == 8),n)

# sample_reds <- reds %>%
#   group_by(quality) %>%
#   sample_n(50, replace = T) %>%
#   ungroup()
# sample_reds

sample_reds <- rbind(reds_3,reds_4,reds_5,reds_6,reds_7,reds_8)
dim(sample_reds)
reds
to.cluster <- scale(sample_reds[1:12])

annotation_row = data.frame(
  Quality = sample_reds$quality,
  Quality.Level = sample_reds$quality.level,
  Alcohol = sample_reds$alcohol
)

rownames(annotation_row) <- row.names(sample_reds[1:11])

pheatmap(to.cluster, 
         cutree_rows = 6, 
         cutree_cols = 3,
         annotation_row = annotation_row) 
?pheatmap
