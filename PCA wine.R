#loading the data
wine <- read.csv(file.choose())
View(wine)
cor(wine)
pcaobj <- princomp(wine,cor = TRUE,scores = TRUE,covmat = NULL)
summary(pcaobj)
loadings(pcaobj)
plot(pcaobj)
biplot(pcaobj)
plot(cumsum(pcaobj$sdev*pcaobj$sdev)*100/(sum(pcaobj$sdev*pcaobj$sdev)),type="b")
pcaobj$scores[,1:3]
wine <- cbind(wine,pcaobj$scores[,1:3])
View(wine)



#hierarchial clustering
clus_data <- wine[,15:17]

norm_clus <- scale(clus_data)
dist1 <- dist(norm_clus,method = "euclidean")
fit1<-hclust(dist1,method="complete")
plot(fit1,hang = -1)
groups<-cutree(fit1,6)
mat<-as.matrix(groups)
View(mat)
final <- cbind(mat,wine)
View(final)
write.csv(final,file="wine_clustered.csv",row.names = F,col.names = F)
getwd()
fit2 <- hclust(dist1,method = "single")
plot(fit2,hang = -1)
groups1 <- cutree(fit2,8)
matr <- as.matrix(groups1)
View(mat)
final1 <- cbind(matr,wine)
write.csv(final1,file = "wine_clustered_single.csv",row.names = F,col.names = F)
#similarly  we can create clusters using the linkage methods "average","centroid".



#K means clustering

clus_data
norm_clus <- scale(clus_data)
dist1 <- dist(norm_clus,method = "euclidean")
library(factoextra)
library(NbClust)
 fviz_nbclust(norm_clus,kmeans,method = "wss") +
  geom_vline(xintercept = NULL,linetype=2) + 
  labs(subtitle = "Elbow method")
#the optimum number of clusters is 4
 
 
 
 
#kmeans clustering on original data

 wineorg <- read.csv(file.choose())
normalizeddt <- scale(wineorg)
fviz_nbclust(normalizeddt,kmeans,method = "wss") +
  geom_vline(xintercept = NULL,linetype=2) + 
  labs(subtitle = "Elbow method")
#optimum number of cluster is 3