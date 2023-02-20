################################################################
library(dplyr)
#1) K-Means Clustering
dataset <- read.csv("D:/....... UOTTWA/Fundemental of applied sci neger/Assignment/4/framingham.csv",header=TRUE)
dataset = dataset[,1:2]
###################################################################removve na
dataset = dataset[,!(names(dataset) %in% c("TBG"))]
dataset=dataset %>% drop_na()
##############################################################
dataset$male <-as.factor(dataset$male)
dataset$age <-as.numeric(dataset$age)

#########data Standatization
dataset$age <- scale(dataset$age)
#########################################################
dataset$male <-as.factor(dataset$male)
dataset$age <-as.numeric(dataset$age)


glimpse(dataset)
str(dataset)
summary(dataset)
table(dataset)
boxplot(dataset$age)


#a) Perform k-means clustering on the selected attributes, specifying k = 4 clusters and plot.

#K-Means Clustering Algorithm
set.seed(917)
#Run k-means cluster of the dataset
Cluster_kmean <- kmeans(dataset, 4, nstart = 20)
#Tabulate the cross distribution
table(Cluster_kmean$cluster,dataset$male)
#Plot
Cluster_kmean$cluster <- factor(Cluster_kmean$cluster)
ggplot(dataset, aes(age,male, color = Cluster_kmean$cluster), ) +
  geom_point(size=3)


#b) Apply the elbow method to determine the best k and plot.
wss <- (nrow(dataset))*sum(apply(dataset,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(dataset,centers=i)$withinss)
}
plot(1:15, wss, type="b",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#c) Evaluate the quality of the clusters using the Silhouette Coefficient method.

library(cluster)
silhouette_score <- function(k){
  km <- kmeans(dataset, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(dataset))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters',
     ylab='Average Silhouette Scores', frame=FALSE)

