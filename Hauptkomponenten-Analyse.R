########## HAUPTKOMPONENTEN ANALYSE
library(factoextra)
library(dplyr)
library(ggbiplot)

# Daten laden
path <- file.path("~/Documents/Data","","wineQualityReds.csv")
reds <- read.csv(path)
reds <- reds %>% mutate(quality.level = ifelse(reds$quality < 5, "low", ifelse(reds$quality < 7, "average", "high")))
reds$quality.level <- factor(reds$quality.level,levels=c("low", "average", "high"), ordered=TRUE)
data <- reds[2:12]
target_category <- reds$quality.level

## PCA durchführen
# prcomp führt automatisch centering durch, für scaling scale=T
pca <- prcomp(data, scale=TRUE, center=TRUE)
summary(pca)
names(pca)
pca$sdev # Standard-Abweichung der einzelnen Komponenten
pca$center
pca$scale

## Loadings der PCs
# => zur Interpretation des Datensets (was mit einander ansteigt, sinkt,...)
pca$rotation

## Spree-Plot anzeigen
# => bestimmen der Anzahl der zu benutzenden PCAs
fviz_eig(pca,addlabels = T)

## Alternativ: berechnen
#..oder Variance explained
(pca.var <- pca$sdev^2)
# => dann Anteil an variance explained
pca.var / sum(pca.var)


## Principal Component Score Vectors
# = multiplizeren der Daten mit mit den PC loading vectors
# => bei prcomp kein explizites multiplizieren notwenig => $x
pca$x


# Biplot 
biplot(pca, scale = 0)
# besser...
fviz_pca_biplot(pca, habillage=as.factor(target_category),
                label ="var", repel = TRUE) + 
                xlim(-5,5.5) + ylim(-4.5,4.5) + theme_minimal()

fviz_pca_var(pca, col.var = "contrib", 
            #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# mit circles 
ggbiplot(pca, ellipse=TRUE, circle = F, alpha =.5, groups = as.factor(reds$quality.level))
  # + scale_colour_manual(name="Quality", values= c("forest green", "steelblue", "dark blue"))

