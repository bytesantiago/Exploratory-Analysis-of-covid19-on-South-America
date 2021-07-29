library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dendextend) # for comparing two dendrograms

my_data <- as.data.frame(owid.covid.data)

new_data[is.na(new_data)] <- 0

my_data$date <- as.Date(my_data$date, format= "%Y-%m-%d")

new_data <- subset(my_data, date > "2020-06-18")

cases_by_cont <- as.data.frame(tapply(new_data$total_cases, new_data$continent, FUN = sum))
colnames(cases_by_cont) <- c("total_cases")

tests_by_cont <- as.data.frame(tapply(new_data$total_tests, new_data$continent, FUN = sum))
colnames(tests_by_cont) <- c("total_test")

new_cases_by_cont <- as.data.frame(tapply(new_data$new_cases, new_data$continent, FUN = sum))
colnames(new_cases_by_cont) <- c("new_cases")

deaths_by_cont <- as.data.frame(tapply(new_data$total_deaths, new_data$continent, FUN = sum))
colnames(deaths_by_cont) <- c("total_deaths")

new_deaths_by_cont <- as.data.frame(tapply(new_data$new_deaths, new_data$continent, FUN = sum))
colnames(new_deaths_by_cont) <- c("new_deaths")

grouped_data <- cbind(cases_by_cont, new_cases_by_cont, deaths_by_cont, new_deaths_by_cont)

grouped_data.scale <- as.data.frame(scale(grouped_data))

#Remove the world variable
grouped_data.scale <- grouped_data.scale[-1,]

#Determine clusters number with griup disimilarity
set.seed(80)

submt <- kmeans(grouped_data.scale,centers = 1)$betweenss

for(i in 1:5) submt[i] <- kmeans(grouped_data.scale,centers = i)$betweenss

plot(1:5, submt, type = "b", xlab = "Cluster's number", ylab = "Sum of inter group squares")

#Kmeans and plot of k-means
covid_kmeans <- kmeans(grouped_data.scale, centers = 4)

fviz_cluster(covid_kmeans, data = grouped_data.scale)

# Dissimilarity matrix
d <- dist(grouped_data.scale, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

#Agnes cluster dendogram
hc3 <- agnes(grouped_data.scale, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

#cluster dendogram with borders
plot(hc1, hang = -1, cex = 0.6)
rect.hclust(hc1, k = 4, border = 2:5)