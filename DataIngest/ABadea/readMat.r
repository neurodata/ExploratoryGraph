require('R.matlab')
require('igraph')

setwd("~/git/ExploratoryGraph/DataIngest/ABadea/")

listGs<- list.files(path = "./raw/", pattern = "*.m")

d<- listGs[1]


mat2graphml<- function(d){
path<-paste("./raw/",d,sep = "")
m<- readMat(path)
g<- graph.adjacency(m$connectivity, mode="undirected", weighted=TRUE)


prefix<- unlist(strsplit(d,split = ".",fixed = T))[1]
outpath<-paste("./graphml/",prefix,".graphml",sep = "")

write.graph(g,file=outpath,format = "graphml")

}

lapply(listGs, mat2graphml)