```{r echo=TRUE,message=TRUE}

require("igraph",quietly = T)
require("networkD3",quietly = T)
require("d3heatmap")
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
degreeGraph<- rowSums(adjacency)
orderByDegree<- order(degreeGraph,decreasing = T)

## Adjacency Matrix: ordered by degree
# image(adjacency[orderByDegree,orderByDegree])
d3heatmap(adjacency[orderByDegree,orderByDegree], scale = "none", colors = ("Blues"))

