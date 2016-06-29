require("igraph",quietly = T)
# require("fields")
require("networkD3")
setwd("~/git/ExploratoryGraph/Code/Functions/")

g <- read.graph("./graphViz/demo.graphml",format = "graphml")


get.graph.attribute(g)
get.vertex.attribute(g)
l <- edge_attr(g)


# 
if(!is.connected(graph)){
  g<-decompose.graph(g)[[1]]
}
g <- simplify(g, remove.multiple = F, remove.loops = T) 

g <- as.undirected(g, mode = "collapse")

# plot(g, edge.arrow.size=1,vertex.label=NA)

vertex_list<- V(g)
edge_list<- E(g)


members <- rep(1, length(V(g)))

graph_d3 <- igraph2D3(g, members)

forceNetwork(
  Links = graph_d3$links,
  Nodes = graph_d3$nodes,
  Source = 'source',
  Target = 'target',
  NodeID = 'name',
  Group = 'group',
  zoom = TRUE
)






#get adjacency
adjacency<- get.adjacency(graph,type = "both",attr = NULL,sparse = T, edges = F)
A = adjacency>0
A <- as.matrix(A)*1

#Compute the degree
D<- rowSums(as.matrix(A)*1)

#Laplacian
L <- D-A
ev_L<-eigen(L)

Re(ev_L$values)


#diffusion
W<- diag(1/D)%*%A
n<- nrow(W)

#p
p<- rep(0,n)
p[1]<-1


plot(p)
c(p%*%W)[1]


