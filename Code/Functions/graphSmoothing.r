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

#Let's compute the adjacency and convert it to a boolean matrix

adjacency<- get.adjacency(graph,type = "both",attr = NULL,sparse = T, edges = F)
A = adjacency>0

#Compute the degree
degreeGraph<- rowSums(as.matrix(A))
orderByDegree<- order(degreeGraph,decreasing = T)

#put a common prior Beta(alpha, beta)
alpha <- 0.5
beta <- 0.5

post_alpha<-  A + alpha
post_beta<- 1- A + alpha
post_A_mean<- post_alpha/(post_alpha+post_beta)

#plot
sortedA<- post_A_mean[orderByDegree,orderByDegree]

d3heatmap(as.matrix(sortedA), scale = "none", colors = ("Blues"))
