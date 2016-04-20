```{r echo= FALSE,message=TRUE}

require("igraph",quietly = T)
require("networkD3",quietly = T)
require("d3heatmap")
require("plotly")
# require("fields")

setwd("~/git/ExploratoryGraph/Code/Functions/")

graph <- read.graph("../DataIngest/p.pacificus_neural.synaptic_1.graphml",format = "graphml")

graph<- as.undirected(graph, mode="collapse")

#Simpel walktrap to cluster the nodes
graphClusters<- cluster_walktrap(graph)
members <- membership(graphClusters)
graph_d3 <- igraph_to_networkD3(graph, group = members)

#Plot it in the fancy & interactive D3!
forceNetwork(Links = graph_d3$links, Nodes = graph_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group')

#Let's compute the adjacency and convert it to a boolean matrix

adjacency<- get.adjacency(graph,type = "both",attr = NULL,sparse = T, edges = F)
adjacency = adjacency>0

#Compute the degree
degreeGraph<- rowSums(as.matrix(adjacency))
orderByDegree<- order(degreeGraph,decreasing = T)

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

d<- 3
svdL<- svd(sortedA,nu = d,nv = d)
# plot(svdL$d)
sigma<- svdL$d[1:d]
X<- svdL$u %*% diag( sqrt(sigma))



#let's do k-means

K = 5
kmeans_X <- kmeans(X,K)

plot_ly(x= X[,1],y= X[,2],z= X[,3], type = "scatter3d",mode = "markers", color=kmeans_X$cluster)


#degree dist
plot_ly(x = degreeGraph,type = "histogram")
