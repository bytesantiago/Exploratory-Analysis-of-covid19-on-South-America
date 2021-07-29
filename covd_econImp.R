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

#data[,c(1:4,5:8,10,12:14,16:19,23:25,28:34)] <- NULL

data[,c(1:2,4,5:8,10,12:14,16:19,23:25,28:34)] <- NULL

#Removing "World" row
row.names.remove <- c("World")
data <- data[!(row.names(data) %in% row.names.remove), ]

#Removing "Falkand Islands" row (For south America)
row.names.remove <- c("Falkland Islands")
data <- data[!(row.names(data) %in% row.names.remove), ]


####GDP Variances in percentages
gdpGrowth2019 <- c(-2.2,2.8,1.1,1.1,3.3,0.1,4.7,0.2,2.2,2.3,0.2,-35)
data$gdp2019 = gdpGrowth2019

gdpGrowth2020 <- c(-5.7,-2.9,-5.3,-4.5,-2.4,-6.3,52.8,-1,-4.5,-4.9,-3,-15)
data$gdp2020 = gdpGrowth2020

gdpGrowth2021 <- c(4.4,2.9,2.9,5.3,3.7,3.9,6.3,4,5.2,4.9,5,-5)
data$gdp2021 = gdpGrowth2021

#####Unemployment Rates
unempRate2019 <- c(9.8,4,11.9,7.3,10.5,3.8,NaN,7.2,6.6,8.6,9.4,NaN)
data$unemRt2019 = unempRate2019

unempRate2020 <- c(10.9,8,14.7,9.7,12.2,6.5,NaN,7.1,7.1,9.5,10.5,NaN)
data$unemRt2020 = unempRate2020

unempRate2021 <- c(10.1,4,13.5,8.9,11.9,5.9,NaN,6.4,7.3,8.6,8.1,NaN)
data$unemRt2021 = unempRate2021

#####Active_cases
activecases <- c(55872,32090,639179,23204,85467,33027,128,1741,98377,240,55,6943)
data$activecases = activecases

data <- as.data.frame(scale(data))

location <- rownames(data)
data$location = location

write.csv(data,"FinalData.csv", row.names = FALSE)

 #normalize <- function(x){(x-min(x))/(max(x)-min(x))}
#data <- normalize(data)
data <- imputePCA(data, ncp = 5)
data <- as.data.frame(data$completeObs)
res.pca <- PCA(data, scale = TRUE, graph = FALSE)
#res.pca <- prcomp(data, scale = TRUE)
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

set.seed(80)

#determine the optimal number of clusters

fviz_nbclust(data, kmeans, method="wss") #metodo codo
#fviz_nbclust(data, kmeans, method="silhouette") #metodo de silueta de promedio

#Determine clusters number with group disimilarity
submt <- kmeans(data,centers = 1)$betweenss

for(i in 1:10) submt[i] <- kmeans(data,centers = i)$betweenss

plot(1:10, submt, type = "b", xlab = "Cluster's number", ylab = "Sum of inter group squares")
#Kmeans
covid_kmeans <- kmeans(data, centers = 4)

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
rect.hclust(hc3, k = 5, border = 2:5)

#####Biplots

#Simetrico

fviz_pca_ind(res.pca) #Individual's biplot

fviz_pca_var(res.pca) #Variable's biplot

fviz_pca_biplot(res.pca, repel = TRUE, axes=c(1,2)) #Both, variables and individuals biplot

var <- get_pca_var(res.pca)
var

ind <- get_pca_ind(res.pca)
ind
