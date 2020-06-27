wine<-read.csv(file.choose()) 
View(wine)

wine_data <- wine[-1]
View(wine_data)

cor(wine_data)
str(wine_data)

# Hierarchical Clustering using hclust function 
?hclust
dist_data_1 <- dist(wine_data, method = "euclidean")
d_hclust<-hclust(dist_data_1,method="complete") 
#  Dendrogram
plot(d_hclust) 

groups_1<-cutree(d_hclust,3) # Cutting the dendrogram for 3 clusters

cluster_num <- as.matrix(groups_1)
View(cluster_num)

## k mean clustering
?kmeans
wine_kmean <- kmeans(wine_data, 3)
#wine$kmean_clust_wine <- as.factor(wine_kmean$cluster)
#wine <- wine[-15]
kmean_num <- as.factor(wine_kmean$cluster)

wine1<-cbind(cluster_num,kmean_num,wine)
View(wine1)
View(aggregate(wine1[,-c(2)],by=list(cluster_num),FUN=mean)) 

###################
# PCA model building

wine_pca<-princomp(wine_data, cor = TRUE, scores = TRUE, covmat = NULL)

str(wine_pca)
summary(wine_pca)
loadings(wine_pca)

# Comp.1 having highest importance
plot(wine_pca)

biplot(wine_pca)

plot(cumsum(wine_pca$sdev*wine_pca$sdev)*100/(sum(wine_pca$sdev*wine_pca$sdev)),type="b")

# Top 3 PCA Scores
wine_pca$scores[,1:3]  

#binding top 3 scores with db
wine1 <- cbind(wine1, wine_pca$scores[,1:3])
View(wine1)

clust_data <- wine1[,17:19]
View(clust_data)

#Normalizing the data 
norm_clust<-scale(clust_data) 

?dist
dist_data<-dist(norm_clust,method = "euclidean") 

# Hierarchical Clustering using hclust function 
?hclust
data_hclust<-hclust(dist_data,method="complete") 
#  Dendrogram
plot(data_hclust) 

groups<-cutree(data_hclust,3) # Cutting the dendrogram for 3 clusters

cluster_no <- as.matrix(groups)
View(cluster_no)

## k mean clustering
?kmeans
data_kmean <- kmeans(dist_data, 3)
#wine$kmean_clust <- as.factor(data_kmean$cluster)
#wine <- wine[-18]
kmean_no <- as.factor(data_kmean$cluster)

final1<-cbind(cluster_no,kmean_no,wine1)
View(final1)
View(aggregate(final1[,-c(2:4,19:21)],by=list(cluster_no),FUN=mean)) 

# Inferences can be
# drawn from the aggregate of the universities data on membership_1

write.csv(final1,file="wine_cluster.csv",row.names = F,col.names = F)
getwd()

ifelse(final1$cluster_no==final1$cluster_num,1,NA)
ifelse(final1$kmean_no==final1$kmean_num,1,NA)

ifelse(final1$cluster_no==final1$cluster_num,1,ifelse(final1$cluster_no==final1$Type,0,NA))
ifelse(final1$kmean_no==final1$kmean_num,1,ifelse(final1$kmean_no==final1$Type,0,NA))


