#installing the required packages
install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("factoextra")

#attaching the mentioned packages
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(factoextra)

#importing the iris data set
dataset <- iris
#checking the lenght of the data set
length(dataset)
#viewing the head of the data set
head(dataset)
#viewing the tail of the data set
tail(dataset)
#structure of the data set
str(dataset)
#summary of the data set
summary(dataset)
#checking the missing values
any(is.na(dataset))

#creating the new data set for k-means clustering
tsf <- dataset [ ,(1:4)]
#viewing the new data set
head(tsf)
tail(tsf)
#determining the optimum number of clusters for k-means clustering
# 1. The Elbow Method
# Wssplot function (within-cluster-sum of squared errors)
wssplot <- function(dataset, nc=15, seed= 1234){
wss <- (nrow(dataset)-1)*sum(apply(dataset,2,var))
for (i in 2:nc){
 set.seed(seed) 
  wss[i] <- sum(kmeans(dataset, centers =i)$withinss)}
 plot(1:nc, wss, type="b", xlab="number of clusters", ylab="within groups sum of squares")
 wss
}
 #calling the wss plot function and displaying the wssplot for data set
wssplot(tsf)
# 2. The Silhouette Method (optimum number of clusters of 
fviz_nbclust(tsf, kmeans, method='silhouette')
#applying k-means clustering, with no  of clusters as 2
KM <- kmeans(tsf, centers = 2)
#viewing the k-means clusters size
KM$size
#viewing the k-means clusters for data set
KM$cluster
##viewing the k-means clusters for centers
KM$centers
#visualizing the clusters
autoplot(KM, tsf, frame= TRUE)

