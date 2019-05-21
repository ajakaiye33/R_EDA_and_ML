##################### Unsupervise Clustering with R ###################

#set working directory
setwd("/Users/ajakaiye/Documents/edvancer/Data")

#load neceaasary packaes
library(dplyr)

# read the data 
winey_data <- read.csv("winequality-red.csv", sep = ";", stringsAsFactors = FALSE)
str(winey_data)

## In an ideal data there would not be labels in the data set(we keep this so as to measure our performance)

# load data only needed in the clustering. remove the "quality variable"
winey_data_cluster <- winey_data %>% select(-quality)
str(winey_data_cluster)

## preproscessing steps in clustering

# step 1: treatment of missing values
# we can identify missing value graphically using VIM function
library(VIM)
aggr(winey_data_cluster) # no missing values

# step 2:  scale variables have values between  +-3
## using the scale function
?scale
winey_data_cluster_scaled <- scale(winey_data_cluster)
View(winey_data_cluster)
View(winey_data_cluster_scaled)
class(winey_data_cluster_scaled)

## apply the kmeans clustering function

?kmeans # import arguements: data, centers(no of cluster), iter.max and nstart
kmeans_result <- kmeans(winey_data_cluster_scaled, centers = 3, iter.max = 25, nstart = 15)

#variables within the kmeans algorithm
str(kmeans_result)
kmeans_result$cluster
kmeans_result$centers
kmeans_result$withinss # within sum of squared ---> Intra-cluster
kmeans_result$betweenss # between sum of squared  ---> Inter- cluster
kmeans_result$tot.withinss# total within sum of square --->

# visualization
library(ggplot2)

winey_data_cluster_scaled <- data.frame(winey_data_cluster_scaled)
winey_data_cluster_scaled$clusternumber = kmeans_result$cluster
View(winey_data_cluster_scaled)
colnames(winey_data_cluster_scaled)

ggplot(winey_data_cluster_scaled,
       aes(fixed.acidity,citric.acid, color = as.factor(clusternumber))) + geom_point()

install.packages("factoextra")
library(factoextra)
fviz_cluster(kmeans_result, data = winey_data_cluster_scaled[,-c(12)], geom = "point") + ggtitle("K-3")

# The essence of clustering does not lie in graphical representation but ON STUDYING THE CLUSTER CENTER(CLUSTER PROFILING)
kmeans_result$centers
# from profilling the cluster centers we can observe the following
#Characteristics of the cluster
# C1-
# HIGH FREE & TOTAL SULFUR DIOXIDE
#VERY LOW ALCOHOL & SULPHATE & PH
#C2
# HIGH pH, High Volatile Acidity
#Low fixed acidity low citric acid

#C3
#High Fixed acidit, High Citric acid
#Low pH, Low volatile acidity

  
#finding out the optimum number of clusters

############### CENTROID BASE CLUSTERING METHODS ######################

## Method 1: Elbow method: This plots different intra cluster distances for different K values
#this method takes  more congizance of intercluster than intra-clusters
# this method is applied in conjunction with business objectives
wss <- function(data,k){
 return(kmeans(data, k, iter.max = 10, nstart = 15)$tot.withinss)
}

k.values <- 1:15
wss_values = vector()
for (i in k.values){
  wss_values[i] <- wss(winey_data_cluster_scaled[,-c(12)],i)
}

plot(k.values,wss_values, type = "b", xlab = "No. of clusters", ylab = "Intra cluster distance")
#thecluster could be between 5 and 8

#though business can aid in selecting the appropriate cluster however we could plot all likely clusters numbers
km_result5 <- kmeans(winey_data_cluster_scaled[,-c(12)], centers = 5,iter.max = 25, nstart = 15)
km_result6 <- kmeans(winey_data_cluster_scaled[,-c(12)], centers = 6,iter.max = 25, nstart = 15)
km_result7 <- kmeans(winey_data_cluster_scaled[,-c(12)], centers = 7,iter.max = 25, nstart = 15)
km_result8 <- kmeans(winey_data_cluster_scaled[,-c(12)], centers = 8,iter.max = 25, nstart = 15)

# get the cluster profile of individual cluster
km_result5$centers #will give characteristics of C1 to c5
#C1
#High alcohol, High Ph & Low density & low fixedacidity
#C2
# Mild Vilacity, Mid ph Law citric acid low & fixed acidy
km_result6$centers

#other method of getting number of cluster

## Method: 2 Average Silhouette Method
?fviz_nbclust
fviz_nbclust(winey_data_cluster_scaled[,-c(12)],kmeans,method = "silhouette")
# with this method, the ideal cluster shoulf be two

## Method 3: Gap_ statistic method ; 
#this method runs Morte Carlo Simulations internally to test which K value gives itself the best separation between clusters and best closeness among clusters
fviz_nbclust(winey_data_cluster_scaled[,-c(12)],kmeans, k.max = 25, method = "gap_stat")
# with the gap_stat method, the ideal cluster should be 2

## Method 4: Ensemble method (using the Nbclust package) of finding out the best number of cluster
install.packages("NbClust") # this method aggregate about 30 methods together
library(NbClust)
NbClust(winey_data_cluster_scaled[,-c(12)],
        diss = NULL, method = "kmeans")
#  according to Nbclust result out of 23 6 proposed 2 as the ideal cluster
# and secondly out of 23, 8 propose 6 as the ideal cluster(so apply both on  kmeans function and compare with biz obj)


####################### DISTANCE/CONNECTIO BASED CLUSTERING METHOD#######################

# Hierachical clustering
# Dendogram- helps us to understand intra cluster distance
# combination of Elbow plot and Dendogram Diagram can help to find out the optimum number of cluster
#How to create a hierachical cluster?

# Method 1: Hierarchical clustering
#  2 diadvantages of  hierarchical cluster: 
#a. Doesn't say anything about the behaviour of the cluster
#b, BECOME UNSCALABLE WHEN HUGE DATA INVOLVE

h_cluster <- hclust(dist(winey_data_cluster_scaled[,-c(12)]))
plot(h_cluster)
#cut the dendrogram
h_cluster_result <- cutree(h_cluster,6)
