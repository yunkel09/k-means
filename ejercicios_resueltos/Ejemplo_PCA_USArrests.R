## ----eval=FALSE--------------------------
## install.packages("FactoMineR")
## install.packages("factoextra")


## ----------------------------------------
library(FactoMineR)
library(factoextra)
data("USArrests")
res.pca <- PCA(USArrests, graph = FALSE)



## ----------------------------------------
eig.val <- res.pca$eig
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by PCs (%)",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")

# agrega una línea al gráfico
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")



## ----------------------------------------
plot(res.pca, choix = "ind", autoLab = "yes")


## ----------------------------------------
plot(res.pca, choix = "var", autoLab = "yes")


## ----------------------------------------
# valores propios (Eigenvalues)
res.pca$eig
  
# Resulados para las variables
res.var <- res.pca$var
res.var$coord          # Coordenadas
res.var$contrib        # Contribuciones a los PC
res.var$cos2           # Calidad de la representación

# Resultados para los indiivduos
res.ind <- res.pca$var
res.ind$coord          # Coordinadas
res.ind$contrib        # Contribuciones al os PC
res.ind$cos2           # Calidad de la representación


## ----------------------------------------
# Calculamos el PCA indicando el número de dimensiones a retener: ncp = 3
res.pca <- PCA(USArrests, ncp = 3, graph = FALSE)

# Calculamos la agrupación sobre los componentes principales
res.hcpc <- HCPC(res.pca, graph = FALSE)



## ----------------------------------------
fviz_dend(res.hcpc, 
          cex = 0.7,                     # tamaño de la etiqueta
          palette = "jco",               # paleta de color, ver más opciones en ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # agregar un rectángulo en los grupos
          rect_border = "jco",           # colorear los rectángulos
          labels_track_height = 0.8      # aumentar las etiquetas
          )


## ----------------------------------------
fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
             )



## ----------------------------------------
# PCA + agrupación cluster
plot(res.hcpc, choice = "3D.map")


## ----------------------------------------
head(res.hcpc$data.clust, 10)


## ----------------------------------------
res.hcpc$desc.var$quanti


## ----------------------------------------
res.hcpc$desc.axes$quanti


## ----------------------------------------
res.hcpc$desc.ind$para

