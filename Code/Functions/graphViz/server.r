require("shiny")
require("plotly")
require("igraph", quietly = T)
source("degreeDist.r")
require("networkD3",quietly = T)
source("opt_d.r")
setwd("~/git/ExploratoryGraph/Code/Functions/graphViz/")

shinyServer(function(input, output) {
  graph <-
    read.graph("../../DataIngest/p.pacificus_neural.synaptic_1.graphml",
               format = "graphml")
  graph <- as.undirected(graph, mode = "collapse")
  
  adjacency <-
    get.adjacency(
      graph,
      type = "both",
      attr = NULL,
      sparse = T,
      edges = F
    )
  A = adjacency > 0
  
  #Compute the degree
  degreeGraph <- rowSums(as.matrix(A))
  orderByDegree <- order(degreeGraph, decreasing = T)
  
  df <- data.frame(x <- degreeGraph, group <- 1)
  
  #densemat
  denseA <- reactive({
    
    denseA<- as.matrix(A)*1
    if(input$adjacency_sortedby=="Degree")
    denseA<- denseA[orderByDegree,orderByDegree]
    return(denseA)
    }
    )
  
  #embedding
  
  sortedA<- A[orderByDegree,orderByDegree]
  sortedD<- degreeGraph[orderByDegree]
  
  D_half<- sqrt((sortedD))
  L <-  diag(1/D_half)%*%as.matrix(sortedA) %*%diag(1/D_half)
  rownames(L) <- rownames(sortedA)
  colnames(L) <- colnames(sortedA)
  
  svdL <- reactive({
    #3D embedding
    d<- input$spectral_d_to_view
    svdL<- svd(sortedA,d,d)
    return(svdL)
  })
  
  spree_ev <- reactive({
    svdL()$d[1:input$spectral_d_to_view]
  })
  
  profiled_lik_optimal_d <- reactive({
    v<- svdL()$d
    opt_d(v)
  })
  
  X<- reactive({
    sigma<- svdL()$d[1:input$spectral_d_to_view]
    X<- svdL()$u %*% diag(sqrt(sigma))
    X
  })
  
  spec_cluster <- reactive({
    kmeans_X <- kmeans(X(),input$cluster_K)
    kmeans_X$cluster
  })
  
  
  # d <- callModule(serverDegreeDist, "d")
  output$density_dist <- renderPlotly({
    if (input$plot_degree) {
      p <- ggplot(df, aes(x)) +
        geom_histogram(
          aes(y = ..density..),
          alpha = 0.7,
          fill = "#333333",
          binwidth = 1
        )
    }
    
    if (input$show_histogram) {
      p <- ggplot(df, aes(x)) +
        geom_histogram(
          aes(y = ..density..),
          alpha = 0.7,
          fill = "#333333",
          binwidth = input$binwidth
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
        ggtitle("Degree Distribution")
      
      # ggplotly(p)
    }
  })
  output$graph_layout <- renderForceNetwork({

    members <- rep(1,length(V(graph)))
    graph_d3 <- igraph_to_networkD3(graph, members)
    
    #Plot it in the fancy & interactive D3!
    forceNetwork(Links = graph_d3$links, Nodes = graph_d3$nodes, Source = 'source', Target = 'target', NodeID = 'name', Group = 'group')
  
  })
  
  output$adjacency_view <-  renderPlotly({
    
    plot_ly(z =denseA() , type = "heatmap",
            colorscale = "blue")
  })
  
  output$spree_plot <-  renderPlotly({
    plot_ly(y =spree_ev())
    })
  
  output$optimal_d <- renderText({ 
    paste("Optimal dimension (suggested by max profiled likelihood), ",profiled_lik_optimal_d(),sep="") })
  
  output$embeded_scatter <-  renderPlotly({
    plot_ly(x= X()[,1],y= X()[,2],z= X()[,3], type = "scatter3d",mode = "markers", color=spec_cluster())
  })
  
  
  
})