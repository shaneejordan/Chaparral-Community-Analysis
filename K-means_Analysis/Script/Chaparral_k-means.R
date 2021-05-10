#K-means clustering with preliminary data
#02/25/2021
#created by Shane E. Jordan

#Clear the environment
rm(list=ls())

#load libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(PNWColors)

#let's start preparing the data
df <- read_csv('C:/Users/pfura/Documents/Microclimatic Refugia of Disjunct Chaparral Relicts in Mountains of the Eastern Mojave Desert/K-means clustering/Data/ChaparralKmeans2.csv')

#take a look at the data structure
names(df)
str(df)

#need to change the variable format
df$ï..Species <- as.character(df$ï..Species)
str(df)

#change column variable as row names
rownames(df) <- c(df$ï..Species)
head(df)

#get rid of non-numeric first column
df2 <- df[,-1]
head(df2)

#final preparation
df2 <- na.omit(df2) #removes NAs #I don't have NAs but this is such a formality...
df2 <- scale(df2) #standardize variables (recommended)
head(df2)

#take a look at  the 'distance'
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#red pairs are dissimilar
#teal pairs are similar

#ok, now we can make the k-means 
k2 <- kmeans(df2, centers = 3, nstart = 25)
str(k2)

#lots of info here
#cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
#centers: A matrix of cluster centers.
#totss: The total sum of squares.
#withinss: Vector of within-cluster sum of squares, one component per cluster.
#tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
#betweenss: The between-cluster sum of squares, i.e. $totss-tot.withinss$.
#size: The number of points in each cluster.

#print results
k2 

#illustrate results with PCA
fviz_cluster(k2, data = df2)

#illustrate by variable
#default was UrbanPop and Murder, but could use others
df2 %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         ï..Species = row.names()) %>%
  ggplot(aes(T1, T2, color = factor(cluster), label = ï..Species)) +
  geom_text() #doesn't work, but maybe this isn't appropriate with my data

#experiment with different number of k clusters
k2 <- kmeans(df2, centers = 2, nstart = 25)
k3 <- kmeans(df2, centers = 3, nstart = 25)
k4 <- kmeans(df2, centers = 4, nstart = 25)
k5 <- kmeans(df2, centers = 5, nstart = 25)
k6 <- kmeans(df2, centers = 6, nstart = 25)

#illustrate results with PCA
fviz_cluster(k6, data = df2) #an example with k=6

# plots to compare
p1 <- fviz_cluster(k2, geom = "point",  data = df2) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df2) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df2) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df2) + ggtitle("k = 5")
p5 <- fviz_cluster(k6, geom = "point",  data = df2) + ggtitle("k = 6")

#arrange all the plots above into a single figure
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, nrow = 2)

###determine appropriate no. of clusters via elbow, average silhouette or gap method

#elbow method
set.seed(123)

#function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df2, k, nstart = 10 )$tot.withinss
}

#compute and plot wss for k = 1 to k = 15
k.values <- 1:15

#extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#same as above in a single function
set.seed(123)

fviz_nbclust(df2, kmeans, method = "wss") #k=6 is the 'bend in the knee' so k=6 is best

#average silhouette method
#function to compute average silhouette for k clusters

#same as above in a single function
fviz_nbclust(df2, kmeans, method = "silhouette") #suggests k=2 is best

#gap method
#compute gap statistic
set.seed(123)
gap_stat <- clusGap(df2, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
#print the result
print(gap_stat, method = "firstmax")

#visualize results
fviz_gap_stat(gap_stat) #suggets k = 1 is best

#it pretty much looks like two clusters: pine-oak woodland and chaparral
#compute k-means clustering with k = 2
set.seed(123)
final <- kmeans(df2, 2, nstart = 25)
print(final)

fviz_cluster(final, data = df2)

fviz_cluster(final, data = df2, repel = TRUE, ellipse = TRUE,show.clust.cent = FALSE, main = FALSE)+
             scale_colour_manual(values = c("dodger blue","red")) +
             scale_fill_manual(values = c("light blue", "orange"))+
             theme_bw()+
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
             geom_vline(xintercept=0, linetype="dashed", color = "black")+ 
             geom_hline(yintercept=0, linetype="dashed", color = "black")+
             ggsave("kmeans.png")

#final output with k = 2
final_output<-df %>% ###seems like I have to use the non-standardized data here
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
final_output

#try ggplot
df %>%
  ggplot(aes(ï..Species, T2:T14, color = final))+
  geom_point()