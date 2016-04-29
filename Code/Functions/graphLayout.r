```{r echo= FALSE,message=TRUE}

require("igraph",quietly = T)
require("networkD3",quietly = T)
require("d3heatmap")
require("plotly")
require("ggplot2")
# require("fields")

setwd("~/git/ExploratoryGraph/Code/Functions/")

graph <- read.graph("../DataIngest/p.pacificus_neural.synaptic_1.graphml",format = "graphml")

graph<- as.undirected(graph, mode="collapse")


#Plot it in the fancy & interactive D3!
forceNetwork(Links = graph_d3$links, Nodes = graph_d3$nodes, Source = 'source', Target = 'target', NodeID = 'name', Group = 'group')

#Simpel walktrap to cluster the nodes
# graphClusters<- cluster_walktrap(graph)
# members <- membership(graphClusters)
members <- rep(1,length(V(graph)))
graph_d3 <- igraph_to_networkD3(graph, members)

#Plot it in the fancy & interactive D3!
forceNetwork(Links = graph_d3$links, Nodes = graph_d3$nodes, Source = 'source', Target = 'target', NodeID = 'name', Group = 'group')

#Let's compute the adjacency and convert it to a boolean matrix

adjacency<- get.adjacency(graph,type = "both",attr = NULL,sparse = T, edges = F)
adjacency = adjacency>0

#Compute the degree
degreeGraph<- rowSums(as.matrix(adjacency))
orderByDegree<- order(degreeGraph,decreasing = T)


#degree dist
# plot_ly(x = degreeGraph,type = "histogram")


df <- data.frame(x <- degreeGraph, group <- 1)

p <- ggplot(df, aes(x)) + 
  geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333", binwidth = 1) + 
  stat_density(fill = "skyblue", alpha = 0.5,adjust= 1) + 
  theme(panel.background = element_rect(fill = '#ffffff')) + 
  ggtitle("Degree Distribution")

ggplotly(p)

## Adjacency Matrix: ordered by degree
# image(adjacency[orderByDegree,orderByDegree])

sortedA<- adjacency[orderByDegree,orderByDegree]
sortedD<- degreeGraph[orderByDegree]
d3heatmap(as.matrix(sortedA), scale = "none", colors = ("Blues"))

D_half<- sqrt((sortedD))
L <-  diag(1/D_half)%*%as.matrix(sortedA) %*%diag(1/D_half)
rownames(L) <- rownames(sortedA)
colnames(L) <- colnames(sortedA)
d3heatmap(L, scale = "none", colors = ("Blues"))

#scree plot

plot_ly(y=svdL$d)

#3D embedding


d<- 30
svdL<- svd(sortedA)
# plot(svdL$d)
sigma<- svdL$d[1:d]
X<- svdL$u %*% diag( sqrt(sigma))



#let's do k-means

K = 5
kmeans_X <- kmeans(X,K)

plot_ly(x= X[,1],y= X[,2],z= X[,3], type = "scatter3d",mode = "markers", color=kmeans_X$cluster)

blockMembership<- kmeans_X$cluster
names(blockMembership)<- rownames(sortedA)

listVertex<- names(V(graph))

listBlockMembership<- as.list(blockMembership)

reorderedBlockMembership<- do.call("c",lapply(listVertex, function(x){listBlockMembership[[x]]}))


graph_d3 <- igraph_to_networkD3(graph,reorderedBlockMembership)

#Plot it in the fancy & interactive D3!
forceNetwork(Links = graph_d3$links, Nodes = graph_d3$nodes, Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group')




