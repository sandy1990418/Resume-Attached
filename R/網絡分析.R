
setwd("D:\\1082\\商業分析\\Lecture6_data")



#set the graph to be a new window
options(device='windows') # device='quartz' for Mac

### undirected networks
## read the network data
# the first column "FAMILY" of the CSV file represents row names
#0 = 沒有聯姻，1=有聯姻
florence=read.csv("florentine.csv", row.names = "FAMILY")
florence=as.matrix(florence) # coerce into a matrix

# print out the adjacency (sub)matrix for the first 5 families
florence[1:5, 1:5]
rowSums(florence)
##

## simple undirected network
#install.packages("igraph")
library("igraph")  # load the package
##變成igraph project
florence=graph.adjacency(florence, mode = "undirected", diag = FALSE)
##degree= 每個有幾個直接連結的數
plot(florence) # plot the graph

#centrality
degree(florence)
closeness(florence)
betweenness(florence)

#plot via centrality
#讓節點根據中心程度變大vertex.size = closeness(florence) * 1000
plot(florence, vertex.size = closeness(florence) * 1000, main = "Closeness")
plot(florence, vertex.size = betweenness(florence), main = "Betweenness")


# modularity
#群內的連結程度比群外的連結程度還要高
c2= cluster_louvain(florence)
plot(c2, florence)
#c2=分群後的網絡
#modularity(c2) = 分群的目標函式
#modularity = 實際連結程度 - random graph的連結程度(越大越好)
modularity(c2)
#simple illustration of modularity
g1 <- graph.formula(A-B-C-A,D-E-F-D,G-H-I-G,A-D-G-A)
V(g1)$grp_good <- c(1,1,1,2,2,2,3,3,3)
V(g1)$grp_bad <- c(1,2,3,2,3,1,3,1,2)
op <- par(mfrow=c(1,2))
plot(g1,vertex.color=(V(g1)$grp_good),vertex.size=20,main="Good Grouping")
plot(g1,vertex.color=(V(g1)$grp_bad),vertex.size=20,main="Bad Grouping")
par(op) # At end of plotting, reset to previous settings



###directed networks
##read the directed network data
twitter=read.csv("twitter-following.csv", stringsAsFactors = FALSE)
test=as.matrix(twitter)
twitter.adj <- graph.edgelist(test)
plot(twitter.adj) # when too dense, visualization can be messy
degree(twitter.adj, mode = "in") #why use network metrics to describe networks
#add network matrix into analysis
senator=read.csv("twitter-senator.csv", stringsAsFactors = FALSE)
senator$indegree=degree(twitter.adj, mode = "in")
senator$outdegree=degree(twitter.adj, mode = "out") 
in.order=order(senator$indegree, decreasing = TRUE)
out.order=order(senator$outdegree, decreasing = TRUE)
# 3 greatest indegree
senator[in.order[1:3], ]
# 3 greatest outdegree
senator[out.order[1:3], ]
#same applies to closeness and betweeness
closeness(twitter.adj, mode = "in")
betweenness(twitter.adj, directed = TRUE)

