########## CLUSTERING AUF PCS
library(factoextra)
library(FactoMineR)

## PCA & Clustern
# PCAs berechnen => nur die ersten drei behalten
res.pca <- PCA(USArrests, ncp = 3, graph = FALSE)
# Hierarchisches Clustering auf PCs anwenden
res.hcpc <- HCPC(res.pca, graph = FALSE)


## Dendogram anzeigen
fviz_dend(res.hcpc, 
          cex = 0.7,                     
          palette = "jco",              
          rect = TRUE, rect_fill = TRUE, # Rechtecke um die Gruppen
          rect_border = "jco",           
          labels_track_height = 0.8)
# => empfiehlt 4 Cluster
fviz_dend(res.hcpc, k=2)

## Cluster visualisieren
res.hcpc[1:2]
fviz_cluster(res.hcpc,
             repel = TRUE,          
             show.clust.cent = TRUE, 
             palette = "jco",        
             ggtheme = theme_minimal(),
             main = "Factor map")

fviz_dend(res.hcpc, cex = 0.5, k = 4, 
          k_colors = "jco", type = "circular")


