require("shiny")
require("plotly")
require("igraph", quietly = T)
source("degreeDist.r")
require("networkD3", quietly = T)
source("opt_d.r")

shinyServer(function(input, output) {
  
  # Read in the graph from graphml file
  graphFile <- reactive({
    inFile<-input$file1
    graph<- read.graph(inFile$datapath,
               format = "graphml")
    if(!is.connected(graph)){
      graph<-decompose.graph(graph)[[1]]
    }
    graph <- as.undirected(graph, mode = "collapse")
    graph
  })
  
  # get adjacency
  Adjacency <- reactive({
    graph <- graphFile()
    a<-get.adjacency(
      graph,
      type = "both",
      attr = NULL,
      sparse = T,
      edges = F
    )
    A = a > 0
    A
  })
  
  # get laplacian
  Laplacian <- reactive({
    A <- Adjacency()
    denseA <- as.matrix(A) * 1
    D<- rowSums(denseA)
    D_half <- sqrt(D) 
    denseA <-  diag(1 / D_half) %*% denseA %*% diag(1 / D_half)
    denseA
  })
  
  #get SVD
  
  SVD <- reactive({
    A <- Adjacency()
    svdL <- svd(A)
    svdL
  })
  
  
  
  #Compute the max coreness
  
  output$KinKcore <- renderUI({
    graph <- graphFile()
    coreness <- graph.coreness(graph, "all")
    maxCoreness <- max(coreness)
    sliderInput(
      inputId = "par_k_cores",
      label = "K:",
      min = 1,
      max = maxCoreness,
      ticks = T,
      step = 1,
      value = 10
    )
  })
  
  #1st viz: layout
  output$graph_layout <- renderForceNetwork({
    graph <- graphFile()
    
    if (!input$use_k_core) {
      members <- rep(1, length(V(graph)))
      graph_d3 <- igraph_to_networkD3(graph, members)
    } else{
      coreness <- graph.coreness(graph)
      maxCoreness <- max(coreness)

      verticesHavingMaxCoreness <- which(coreness >= input$par_k_cores)
      kcore <-
        induced.subgraph(graph = graph, vids = verticesHavingMaxCoreness)
      members <- rep(1, length(V(kcore)))
      graph_d3 <- igraph_to_networkD3(kcore, members)
    }
    forceNetwork(
      Links = graph_d3$links,
      Nodes = graph_d3$nodes,
      Source = 'source',
      Target = 'target',
      NodeID = 'name',
      Group = 'group'
    )
  })
  
  #2nd viz:vertex stats
  output$density_dist <- renderPlotly({
    graph <- graphFile()
    A <- Adjacency()
    
    if (input$plot_degree) {
      if(input$vertex_stats=="Degree"){
        bw <- degree(graph)
        df <- data.frame(x <- bw, group <- 1)
        min_binwidth = 1
      }
      if(input$vertex_stats=="Betweenness Centrality"){
        bw <- betweenness(graph)
        df <- data.frame(x <- bw, group <- 1)
        min_binwidth = (quantile(bw,0.6)-quantile(bw,0.4))/20
        }
      if(input$vertex_stats=="Closeness Centrality"){
        bw <- closeness(graph)
        df <- data.frame(x <- bw, group <- 1)
        min_binwidth = (quantile(bw,0.9)-quantile(bw,0.1))/10
      }
      if(input$vertex_stats=="Eigenvector Centrality"){
        eg<- evcent(graph)
        bw <- eg$vector
        df <- data.frame(x <- bw, group <- 1)
        min_binwidth = (quantile(bw,0.9)-quantile(bw,0.1))/10
      }
      

        p <- ggplot(df, aes(x)) +
          geom_histogram(
            aes(y = ..density..),
            alpha = 0.7,
            fill = "#333333",
            bins = 10
          )
      
     
    }
    
    if (input$show_histogram) {
      p <- ggplot(df, aes(x)) +
        geom_histogram(
          aes(y = ..density..),
          alpha = 0.7,
          fill = "#333333",
          binwidth = input$binwidth*min_binwidth/10
        )
    }
    
    if (input$density) {
      p <-
        p + stat_density(fill = "skyblue",
                         alpha = 0.5,
                         adjust = input$bw_adjust)
    }
    
    if (input$plot_degree) {
      p <- p +
        theme(panel.background = element_rect(fill = '#ffffff')) +
        ggtitle(input$vertex_stats)
      
      ggplotly(p)
    }
  })
  
  #3rd viz: adjacency
  output$adjacency_view <-  renderPlotly({
    A <- Adjacency()
    
    if(input$adjacency_mode=="Adjacency"){
      denseA <- as.matrix(A) * 1
    }
    
    if(input$adjacency_mode=="Laplacian"){
      denseA<- Laplacian()
    }
    
    if (input$adjacency_sortedby == "Degree")   {
      orderByDegree<- order(rowSums(denseA),decreasing = T)
      denseA <- denseA[orderByDegree, orderByDegree]
    } 
    
    plot_ly(z = denseA,
            type = "heatmap",
            colorscale = "blue")
  })
  
  #4th viz: spree plot
  output$spree_plot <-  renderPlotly({
    
    svdL<- SVD()
    spree_ev <- svdL$d[1:input$spectral_d_to_view]
    
    plot_ly(y = spree_ev)
  
    })
  
  
  
# 
#   
#   
#   
#   
#   if(FALSE){
#   #Compute the degree
# 
#   orderByDegree <- reactive({
#     order(degreeGraph(), decreasing = T)
#     })
#   
#   df <- data.frame(x <- degreeGraph(), group <- 1)
#   
# 
#   #densemat
#   denseA <- reactive({
#    
#   })
#   
#     {
#   #embedding
#   
#   sortedA <- A[orderByDegree, orderByDegree]
#   sortedD <- degreeGraph[orderByDegree]
#   
#   D_half <- sqrt((sortedD))
#   L <-  diag(1 / D_half) %*% as.matrix(sortedA) %*% diag(1 / D_half)
#   rownames(L) <- rownames(sortedA)
#   colnames(L) <- colnames(sortedA)
#   
#   svdL <- svd(sortedA)
#   
#   spree_ev <- reactive({
#     svdL$d[1:input$spectral_d_to_view]
#   })
#   
#   profiled_lik_optimal_d <- reactive({
#     v <- svdL$d
#     opt_d(v)
#   })
#   
#   
#   sigma <- svdL$d
#   X <- svdL$u %*% diag(sqrt(sigma))
#   
#   
#   # d <- callModule(serverDegreeDist, "d")
# 
#   
#   
# 
#   
#   
#   
#   
#   
#  
#     

# 

#   
# 
#   output$optimal_d <- renderText({
#     paste(
#       "Optimal dimension (suggested by max profiled likelihood), ",
#       profiled_lik_optimal_d(),
#       sep = ""
#     )
#   })  
# 
#   if(FALSE){
#     
#   spec_cluster <- reactive({
#       kmeans_X <- kmeans(X[,input$spectral_d_to_view], input$cluster_K)
#       kmeans_X$cluster
#     })
#     
#   output$embeded_scatter <-  renderPlotly({
#     plot_ly(
#       x = X[, 1],
#       y = X[, 2],
#       z = X[, 3],
#       type = "scatter3d",
#       mode = "markers"
#       # ,
#       # color = spec_cluster()
#     )
#   })
#   }
  
  
})