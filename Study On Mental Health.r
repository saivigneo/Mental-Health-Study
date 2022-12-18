library(dplyr)
library(factoextra)
library(devtools)
library(ggbiplot)
install_github("vqv/ggbiplot")



data<-read.csv("D:\\R\\MentalHealth.csv")
#To omit NULL VALUES
data = na.omit(data)
#Excluding 2nd column which is gender column
data = data[,-1]

#Compute PCA and finding out the PC's
mentalhealthpr <- prcomp(data,scale=TRUE)
#head(mentalhealthpr)
summary(mentalhealthpr)

#Plotting is done to see whether there are nay variations after normalizing the data
#A linear graph is plotted to see the variances of the PC's
plot(mentalhealthpr,type="l")

#To plot biplot
g <- ggbiplot(mentalhealthpr,obs.scale = 1, var.scale = 1, groups = data$gender,ellipse = TRUE,circle = TRUE,)

biplot(mentalhealthpr,scale=0)

#PCA results
eig.value.rate<-get_eigenvalue(mentalhealthpr)
print(eig.value.rate)

# variance explained by each principle component
mentalhealthpr$sdev^2 / sum(mentalhealthpr$sdev^2)

# scree plot - a plot that displays the total variance (in %)
fviz_eig(mentalhealthpr)

#Extracting PC Score
print(mentalhealthpr$x)

# PCA binded plot with the original data
rate_merge_pc<-cbind(data,mentalhealthpr$x)
km <- kmeans(data, centers = 2)
#data <- data|> mutate(Cluster=Clusters$Cluster)
#data |> ggplot(aes(x= gender, y= rate, col=as.factor(cluster)))+ geom_point()

print(km)

# Visualize the clusters
fviz_cluster(km, data)
