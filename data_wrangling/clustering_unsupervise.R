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
str(wine_data_cluster)

## preproscessing steps in clustering

# step 1: treatment of missing values
# we can identify missing value graphically using VIM function
library(VIM)
aggr(winey_data_cluster) # no missing values

# step 2:  scale variables
## using the scale function
?scale
winey_data_cluster_scaled <- scale(winey_data_cluster)
View(winey_data_cluster)
View(winey_data_cluster_scaled)

## apply the kmeans clustering function

?kmeans
kmeans_result <- kmeans(winey_data_cluster_scaled, centers = 25, iter.max = 3, nstart = 15)

#variables within the kmeans algorithm
str(kmeans_result)
kmeans_result$cluster
kmeans_result$centers
kmeans_result$withinss # withinsquaredsum ---> Intra-cluster
kmeans_result$betweenss # between squared sum ---> Inter- cluster
kmeans_result$tot.withinss