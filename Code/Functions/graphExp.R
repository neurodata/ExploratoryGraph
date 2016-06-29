require("igraph",quietly = T)
# require("fields")

setwd("~/git/ExploratoryGraph/Code/Functions/")

graph <- read.graph("../DataIngest/p.pacificus_neural.synaptic_1.graphml",format = "graphml")

# 
# if(!is.connected(graph)){
#   graph<-decompose.graph(graph)[[1]]
# }

graph <- as.undirected(graph, mode = "collapse")


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


