setwd("D:\\1082\\商業分析\\Lecture 3_data")




##Introduction to Hierarchical Clustering
#Analyze movieLens data
movies = read.table("movieLens.txt", header=FALSE, sep="|", quote="\"")

str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", 
                     "IMDB", "Unknown", "Action", "Adventure", "Animation",
                     "Childrens", "Comedy", "Crime", "Documentary", "Drama", 
                     "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", 
                     "Romance", "SciFi", "Thriller", "War", "Western")

str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data again:
str(movies)


# Compute distances
distances = dist(movies[,2:20], method = "euclidean")

# Hierarchical clustering(評量點和群的距離用centroid)
clusterMovies = hclust(distances, method = "centroid") 
# Plot the dendrogram
plot(clusterMovies)

# Assign points to clusters
clusterGroups = cutree(clusterMovies, k =10)
table(clusterGroups)

clusterMovies = hclust(distances, method = "ward.D") 

#Now let's figure out what the clusters are like.
# Let's use the tapply function to compute the percentage of movies in each genre and cluster
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)


# Find which cluster Men in Black is in.

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]

# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]



##Introduction to k-means Clustering
#Analyze CharlesBookClub
library(data.table)
CharlesBookClub<-fread("CharlesBookClub.csv")

Book.df=CharlesBookClub[,c(3:6,8:18)]
set.seed(123)

##scale函數=>自動變成標準化後的資料
##centers=分成幾群
Member.seg = kmeans(scale(Book.df), centers=3)


##n start = 隨機亂派幾次
##iter.max=亂派後要重複跑幾次
Member.seg = kmeans(scale(Book.df), centers=3,nstart = 25,iter.max=30)



Member.seg
##各群有的樣本數
Member.seg$size
##每群每個維度(變數)的群內平均數
Member.seg$centers
Member.seg$cluster
##組間變異量
Member.seg$tot.withinss

k=10
WGSS=c()
for(i in 1:k){
   WGSS[i]=sum(kmeans(scale(Book.df), centers=i)$withinss)
}

plot(1:k, WGSS, type="b")


