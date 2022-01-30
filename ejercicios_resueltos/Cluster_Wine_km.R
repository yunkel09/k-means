## ----eval=F, echo=F----------------------
## if(!require(devtools)) install.packages("devtools")
## devtools::install_github("kassambara/factoextra")
## 
## list.of.packages <- c("candisc","cluster", "fpc", "NbClust","car")
## new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
## if(length(new.packages)) install.packages(new.packages)
## 
## 


## ----message=FALSE-----------------------
library(candisc)
data(Wine)
summary(Wine)



## ----------------------------------------
Wine_std <- scale(Wine[,-1]) 


## ----message=FALSE, error=FALSE,warning=FALSE, fig.height=5,fig.width=8----
library(GGally)
ggpairs(Wine[,1:5], aes(colour = Cultivar), progress = FALSE)


## ----------------------------------------
library(clustertend)
set.seed(123)
hopkins(Wine_std, n = nrow(Wine_std) - 1)


## ----fig.height=4,fig.width=6------------
Wine_dist <- dist(Wine_std)
library(factoextra)
fviz_dist(Wine_dist, lab_size =.1, show_labels = FALSE )


## ----fig.height=4,fig.width=8------------
library(NbClust)
set.seed(123)
nb <- NbClust(Wine_std, 
              min.nc = 2,
              max.nc = 10, 
              method = "kmeans", 
              index ="all")



## ----------------------------------------
set.seed(123)
Wine_km <- kmeans(Wine_std, centers = 3)



## ----------------------------------------
Wine_km
table(Wine_km$cluster)


## ----fig.height=8, fig.width=8-----------
library(flexclust)
set.seed(123)
cl_kcca <- as.kcca(Wine_km, Wine_std) 
barplot(cl_kcca)



## ----fig.height=8, fig.width=8-----------
barplot(cl_kcca, bycluster=FALSE)


## ----fig.height=2.5, fig.width=4, message=FALSE, warning=FALSE----
# devtools::install_github("o1iv3r/FeatureImpCluster")
library(FeatureImpCluster)
importance<-FeatureImpCluster(cl_kcca,as.data.table(Wine_std))
plot(importance)



## ----fig.height=7,fig.width=8------------


library(ggrepel)
fviz_cluster(Wine_km, Wine_std,  
             labelsize = 5,
             main="k=3 grupos",
             geom = "point") +
    geom_text_repel(label=paste(Wine$Cultivar, 1:nrow(Wine), sep="_"),
                    size = 1.5,
                    colour = c("darkgreen","blue", "red")[Wine$Cultivar]) 




## ----fig.height=5,fig.width=5------------
library(cluster)
  sk <- silhouette(Wine_km$cluster, Wine_dist) # gráfico de silueta
  plot(sk, main="Silhouette plot - Kmeans", 
       cex.names=0.8, col=1:3, nmax=100)


## ----------------------------------------
table(Wine$Cultivar, Wine_km$cluster)


## ----------------------------------------
library(fpc)
rand<-cluster.stats(d= Wine_dist,
                     alt.clustering=as.numeric(Wine$Cultivar), 
                    clustering=as.numeric(Wine_km$cluster))
rand$corrected.rand

