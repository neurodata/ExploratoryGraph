require("shiny")
require("plotly")
require("igraph", quietly = T)
source("degreeDist.r")
require("networkD3", quietly = T)
source("opt_d.r")

shinyServer(function(input, output) {
  graph <-
    read.graph("./p.pacificus_neural.synaptic_1.graphml",
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
    denseA <- as.matrix(A) * 1
    if (input$adjacency_sortedby == "Degree")
      denseA <- denseA[orderByDegree, orderByDegree]
    return(denseA)
  })
  
  #embedding
  
  sortedA <- A[orderByDegree, orderByDegree]
  sortedD <- degreeGraph[orderByDegree]
  
  D_half <- sqrt((sortedD))
  L <-  diag(1 / D_half) %*% as.matrix(sortedA) %*% diag(1 / D_half)
  rownames(L) <- rownames(sortedA)
  colnames(L) <- colnames(sortedA)
  
  svdL <- reactive({
    #3D embedding
    d <- input$spectral_d_to_view
    svdL <- svd(sortedA, d, d)
    return(svdL)
  })
  
  spree_ev <- reactive({
    svdL()$d[1:input$spectral_d_to_view]
  })
  
  profiled_lik_optimal_d <- reactive({
    v <- svdL()$d
    opt_d(v)
  })
  
  X <- reactive({
    sigma <- svdL()$d[1:input$spectral_d_to_view]
    X <- svdL()$u %*% diag(sqrt(sigma))
    X
  })
  
  spec_cluster <- reactive({
    kmeans_X <- kmeans(X(), input$cluster_K)
    kmeans_X$cluster
  })
  
  
  # d <- callModule(serverDegreeDist, "d")
  output$density_dist <- renderPlotly({
    if (input$plot_degree) {
      if(input$vertex_stats=="Degree"){
      df <- data.frame(x <- degreeGraph, group <- 1)
      min_binwidth = 1
      }
      if(input$vertex_stats=="Betweenness Centrality"){
        bw <- betweenness(graph)
        df <- data.frame(x <- bw, group <- 1)
        min_binwidth = (quantile(bw,0.9)-quantile(bw,0.1))/10
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
          binwidth = min_binwidth
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
  
  
  output$KinKcore <- renderUI({
    coreness <- graph.coreness(graph)
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
  
  
  output$graph_layout <- renderForceNetwork({
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
  
  output$adjacency_view <-  renderPlotly({
    plot_ly(z = denseA() ,
            type = "heatmap",
            colorscale = "blue")
  })
  
  output$spree_plot <-  renderPlotly({
    plot_ly(y = spree_ev())
  })
  
  output$optimal_d <- renderText({
    paste(
      "Optimal dimension (suggested by max profiled likelihood), ",
      profiled_lik_optimal_d(),
      sep = ""
    )
  })
  
  output$embeded_scatter <-  renderPlotly({
    plot_ly(
      x = X()[, 1],
      y = X()[, 2],
      z = X()[, 3],
      type = "scatter3d",
      mode = "markers",
      color = spec_cluster()
    )
  })
  
  
  
})