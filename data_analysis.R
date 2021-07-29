#Loading necessary libraries
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dendextend) # for comparing two dendrograms
library(caret)
library(FactoMineR)
library(ca)
library(ade4)
library(MASS)
library(ExPosition)
library(gplots)
library(corrplot) 
library(scales)
library(NbClust)
library(dplyr)
library(fmsb)
library(ClustOfVar)
library(mice)
library(missMDA)

data <- as.data.frame(dataset)

data$date <- as.Date(data$date, format= "%Y-%m-%d")

data <- subset(data, date == "2020-07-09" )

rownames(data) <- data[,3]

data[,c(1:4,14:19)] <- NULL
data <- as.data.frame(scale(data))

#normalize <- function(x){(x-min(x))/(max(x)-min(x))}
#data <- normalize(data)
data <- imputePCA(data, ncp = 5)
data <- as.data.frame(data$completeObs)
res.pca <- PCA(data, scale = TRUE, graph = FALSE)
res.pca <- prcomp(data, scale = TRUE)
fviz_contrib(res.pca, choice = "var", axes = 1:4, top =10)
summary(res.pca)

#Remove not relevant variables (for all world)
cols_to_keep <- c(9,18,16,22,14,15,13,1,2,21)
data <- subset(data, select = cols_to_keep)

#Remove not relevant variables (for south america)
#cols_to_keep <- c(9,22,14,2,11,4,1,3,8,13)  
cols_to_keep <- c(1,4,6,9,13,14,16,20,21,24)
data <- subset(data, select = cols_to_keep)

#Remove not relevant variables (for north america)
cols_to_keep <- c(23,14,10,13,15,3,2,11,1)
data <- subset(data, select = cols_to_keep)

#Removing "World" row
row.names.remove <- c("World")
data <- data[!(row.names(data) %in% row.names.remove), ]

set.seed(80)

#determine the optimal number of clusters

fviz_nbclust(data, kmeans, method="wss") #metodo codo
#fviz_nbclust(data, kmeans, method="silhouette") #metodo de silueta de promedio

#Determine clusters number with group disimilarity
submt <- kmeans(data,centers = 1)$betweenss

for(i in 1:12) submt[i] <- kmeans(data,centers = i)$betweenss

plot(1:12, submt, type = "b", xlab = "Cluster's number", ylab = "Sum of inter group squares")
#Kmeans
covid_kmeans <- kmeans(data, centers = 6)

fviz_cluster(covid_kmeans, data = data)

# Dissimilarity matrix
d <- dist(data, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

hc3 <- agnes(data, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

#Hierarchical cluster with borders
rect.hclust(hc3, k = 6, border = 2:5)

#####Biplots

#Simetrico

fviz_pca_biplot(PCA(data, graph = FALSE), repel = TRUE) 
