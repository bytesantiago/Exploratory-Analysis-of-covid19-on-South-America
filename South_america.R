library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dendextend) # for comparing two dendrograms

library(FactoMineR)
library(ca)
library(ade4)
library(MASS)
library(ExPosition)
library(gplots)
library(corrplot) 

my_data <- as.data.frame(south_america)

my_data[is.na(my_data)] <- 0

my_data$date <- as.Date(my_data$date, format= "%Y-%m-%d")
new_data <- subset(my_data, date > "2020-06-16")

cases_by_cont <- as.data.frame(tapply(new_data$total_cases, new_data$location, FUN = sum))
colnames(cases_by_cont) <- c("total_cases")

tests_by_cont <- as.data.frame(tapply(new_data$total_tests, new_data$location, FUN = sum))
colnames(tests_by_cont) <- c("total_test")

new_cases_by_cont <- as.data.frame(tapply(new_data$new_cases, new_data$location, FUN = sum))
colnames(new_cases_by_cont) <- c("new_cases")

deaths_by_cont <- as.data.frame(tapply(new_data$total_deaths, new_data$location, FUN = sum))
colnames(deaths_by_cont) <- c("total_deaths")

new_deaths_by_cont <- as.data.frame(tapply(new_data$new_deaths, new_data$location, FUN = sum))
colnames(new_deaths_by_cont) <- c("new_deaths")

grouped_data <- cbind(cases_by_cont, new_cases_by_cont, deaths_by_cont, new_deaths_by_cont)

grouped_data[is.na(grouped_data)] <- 0

grouped_data <- grouped_data[!(row.names(grouped_data) %in% row.names.remove), ]

#Removing "World" row
grouped_data.scale <- as.data.frame(scale(grouped_data))

row.names.remove <- c("Falkland Islands")

grouped_data.scale <- grouped_data.scale[!(row.names(grouped_data.scale) %in% row.names.remove), ]

set.seed(80)

#Determine clusters number with griup disimilarity
submt <- kmeans(grouped_data.scale,centers = 1)$betweenss

for(i in 2:12) submt[i] <- kmeans(grouped_data.scale,centers = i)$betweenss

plot(2:12, submt, type = "b", xlab = "Cluster's number", ylab = "Sum of inter group squares")
#Kmeans
covid_kmeans <- kmeans(grouped_data.scale, centers = 4)

fviz_cluster(covid_kmeans, data = grouped_data.scale)

#Apply PCA to see weights of each varaible 

# Dissimilarity matrix
d <- dist(grouped_data.scale, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

hc3 <- agnes(grouped_data.scale, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

#Hierarchical cluster with borders
plot(hc3, hang = -1, cex = 0.6)
rect.hclust(hc3, k = 5, border = 2:5)

#Computing the correspondence analysis
res.ca<-ca(grouped_data)

#Simetrico
fviz_ca_biplot(res.ca, repel = TRUE) 

#Asimetrico
fviz_ca_biplot(res.ca, map ="rowprincipal", arrow = c(TRUE, TRUE), repel = TRUE) 

#Contribucion Biplot
fviz_ca_biplot(res.ca, map ="colgreen", arrow = c(TRUE, FALSE), repel = TRUE) 