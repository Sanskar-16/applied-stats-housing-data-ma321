source('part-3.R')

source('part-1.R')

print('yooo part 4 here')

#Our question is on if the data naturally falls into clusters based on neighborhoods
#We will be comparing the cluster results to the clustered form by OverallCond

#first, a numeric version of the data
house.data.num = house.data

#this loops turns all numeric columns into factors
for (i in names(house.data.num)) {
  if (is.factor(house.data.num[[i]])) {
    house.data.num[[i]] = as.numeric(house.data.num[[i]])
  }
}

#We can employ K-Means to see if there are any inherit groups within the data

#lets start by doing a PCA transformation using the correlation matrix over the covariance
x.pca = princomp(house.data.num, cor = TRUE)
x.pca.test = prcomp(house.data.num)
#summaries of the transformation
s = summary(x.pca)
s
s$sdev

# plot the Cumulative  contribution of components 
plot(cumsum(s$sdev^2 / sum(s$sdev^2)), type="l", xlab="number of components", ylab="cumulative varince")

#biplot of the first two components 
biplot(x.pca, scale = 0) # no scaling in the plot 

#looking at the rotations/loadings for the 1st and 2nd PCA's
x.pca$loadings[,1]
x.pca$loadings[,2]

#now we can plot the data across the 1st 2 PC's and colour by OverCond and then neighbourhood
proj = as.data.frame(x.pca$scores)
proj$OverallCond = house.data$OverallCond
proj$Neighborhood = house.data$Neighborhood

#the OverallCond Plot
dev.off()
ggplot(proj) + 
  geom_point(aes(x=proj[,1], y=proj[,2], color=OverallCond)) +
  labs(color = "Overall Condition", x = "PC1", y = "PC2", title = "PCA Plot: By Overall Condition")

#The Neighbourhood Plot
ggplot(proj) + 
  geom_point(aes(x=proj[,1], y=proj[,2], color=Neighborhood)) +
  labs(color = "Neighborhood", x = "PC1", y = "PC2", title = "PCA Plot: By Neighborhood")

#We will use the PC's since it explains our data in better dimessions
#now we can have a look at these groups though a k-means clustering algorithm, k = the number of levels in OverallCond
#to make results reproducible, we set the random seed so the center points picked are always the same
set.seed(1)
km.OverallCond = kmeans(x.pca$scores, centers= length(unique(house.data$OverallCond)) ,1000)
km.OverallCond$cluster

#we can table the results with the OverallCond variable
table(km.OverallCond$cluster,as.matrix(house.data$OverallCond))

#observations per cluster
margin.table(table(km.OverallCond$cluster,as.matrix(house.data$OverallCond)), margin=1)

#we can plot the boxplots of the first 3 clusters across the SalePrice and OverallCond (SalePrice is scaled)
par(mfrow = c(3,1))
plot(house.data[km.OverallCond$cluster==1,c(11,44)],pch="x", ylim=c(-3,3))
plot(house.data[km.OverallCond$cluster==2,c(11,44)],col="firebrick", ylim=c(-3,3))
plot(house.data[km.OverallCond$cluster==3,c(11,44)],col="skyblue", ylim=c(-3,3))
par(mfrow = c(1,1))

#now we can have a look at these groups though a k-means clustering algorithm, k = the number of levels in Neighbourhood
set.seed(1)
km.Neighborhood = kmeans(x.pca$scores, centers=length(unique(house.data$Neighborhood)) ,1000)
km.Neighborhood$cluster

#we can table the results with the OverallCond variable
table(km.Neighborhood$cluster,as.matrix(house.data$Neighborhood))

#observations per cluster
margin.table(table(km.Neighborhood$cluster,as.matrix(house.data$Neighborhood)), margin=1)

#we can plot the boxplots of the first 3 clusters across the SalePrice and Neighborhood (SalePrice is scaled)
plot(house.data[km.Neighborhood$cluster==1,c(5,44)], ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==2,c(5,44)],col="firebrick", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==3,c(5,44)],col="skyblue", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==4,c(5,44)],col="khaki", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==5,c(5,44)],col="green", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==6,c(5,44)],col="magenta", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==7,c(5,44)],col="coral4", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==8,c(5,44)],col="purple", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==9,c(5,44)],col="salmon", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.Neighborhood$cluster==10,c(5,44)],col="pink", ylim=c(-3,3), xaxt = "n", xlab = "")

#adding in better x-axis
text(x = 1:length(unique(house.data$Neighborhood)),
     y = par("usr")[3] - 0.1,
     labels = unique(house.data$Neighborhood),
     xpd = NA,
     adj = 1,
     srt = 55
)

#adding in rect to highlight a how the SalePrice varies per a neighbourhood per cluster
rect(xleft = 7.5, xright = 8.5, ybottom = -2, ytop = 2, border = "red", lwd = 3)

#as a final experiment, lets look at how the OverallCond K-Means cluster model looks over neighborhood
#(basically K-Means with 3 centers)
plot(house.data[km.OverallCond$cluster==1,c(5,44)], ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.OverallCond$cluster==2,c(5,44)],col="firebrick", ylim=c(-3,3), xaxt = "n", xlab = "")
par(new = T)
plot(house.data[km.OverallCond$cluster==3,c(5,44)],col="skyblue", ylim=c(-3,3), xaxt = "n", xlab = "")

#adding in better x-axis
text(x = 1:length(unique(house.data$Neighborhood)),
     y = par("usr")[3] - 0.1,
     labels = unique(house.data$Neighborhood),
     xpd = NA,
     adj = 1,
     srt = 55
)

#adding in rect to highlight clusters
rect(xleft = 0, xright = 25, ybottom = -0.5, ytop = 1.2, border = "black")
rect(xleft = 4, xright = 24, ybottom = 0, ytop = 3, border = "red")
rect(xleft = 2, xright = 24, ybottom = -1.5, ytop = 0.2, border = "blue")

#the above shows us that we seem to have 3 recognizable clusters for Neighborhoods, nice!

#now we can have a look at hierarchical clustering, first the libraries
library(HSAUR2)
library(ISLR)

#fitting the hierarchical cluster model
set.seed(1)
hclust.house = hclust(dist(x.pca$scores), method="complete") # aggomorative hierarchical clustering based on complete linkage 

#ploting with the OverallCond labels as the labels
plot(hclust.house,labels=(as.character(house.data$OverallCond)), main="",xlab="complete-linkage",ylab="level")

#ploting squares around the desired clusters (we want the amount of Conditions as the amount of clusters)
rect.hclust(hclust.house,k=length(unique(house.data$OverallCond)), border = "red")

#we can get a table of the results as well
qualClus = cutree(hclust.house, length(unique(house.data$OverallCond)))
table(qualClus, house.data$OverallCond)

#observations per cluster
margin.table(table(qualClus, house.data$OverallCond), margin=1)

#ploting with the neighborhood as the labels
plot(hclust.house,labels=(as.character(house.data$Neighborhood)), main="",xlab="complete-linkage",ylab="level")

#ploting squares around the desired clusters (we want the amount of neighbors as the amount of clusters)
rect.hclust(hclust.house,k=length(unique(house.data$Neighborhood)), border = "red")

#we can get a table of the results aswell
neighClus = cutree(hclust.house, length(unique(house.data$Neighborhood)))
table(neighClus, house.data$Neighborhood)

#observations per cluster
margin.table(table(neighClus, house.data$Neighborhood), margin=1)

#as a final experiment, let's loop though 3 - 24 to see what best clusters we get for neighborhood
####UNCOMMENT THIS SETTING TO SEE ALL THE GRAPHS####
#par(ask = T)
for(i in 3:24){
  
  #plot graph
  plot(hclust.house,labels=(as.character(house.data$Neighborhood)), main="",xlab="complete-linkage",ylab="level")
  
  #plot the labelled clusters
  rect.hclust(hclust.house,k=i, border = "red")
  
  #cut the tree at this cluster amount and table the results
  neighClus = cutree(hclust.house, i)
  table(neighClus, house.data$OverallCond)
  
  #observations per cluster
  margin.table(table(neighClus, house.data$Neighborhood), margin=1)
  
}
