igraph2D3<- function (g, group, what = "both") 
{

if (!("igraph" %in% class(g))) 
  stop("g must be an igraph class object.", call. = FALSE)
if (!(what %in% c("both", "links", "nodes"))) 
  stop("what must be either \"nodes\", \"links\", or \"both\".", 
       call. = FALSE)

temp_nodes <- V(g) %>% as.matrix %>% data.frame
temp_nodes$name <- row.names(temp_nodes)
names(temp_nodes) <- c("id", "name")
temp_nodes$id <- temp_nodes$id - 1
nodes <- temp_nodes$name %>% data.frame %>% setNames("name")

if (!missing(group)) {
  group <- as.matrix(group)
  if (nrow(nodes) != nrow(group)) 
    stop("group must have the same number of rows as the number of nodes in g.", 
         call. = FALSE)
  nodes <- cbind(nodes, group)
}
row.names(nodes) <- NULL
links <- as_data_frame(g, what = "edges")
links <- merge(links, temp_nodes, by.x = "from", by.y = "name")
links <- merge(links, temp_nodes, by.x = "to", by.y = "name")
# if (ncol(links) == 5) {
  # links <- links[, c("id.x", "id.y", "value")] %>% setNames(c("source", 
                                                              # "target", "value"))
# }
# else {
  links <- links[, c("id.x", "id.y")] %>% setNames(c("source", 
                                                     "target"))
# }
if (what == "both") {
  return(list(links = links, nodes = nodes))
}
else if (what == "links") {
  return(links)
}
else if (what == "nodes") {
  return(nodes)
}
}
