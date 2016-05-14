require("shiny")
require("plotly")
require("igraph", quietly = T)
require("networkD3", quietly = T)
source("opt_d.r")
require("pairsD3")
require("plyr")
require("reshape")

shinyServer(function(input, output) {
  
  # Read in the graph from graphml file
  graphFile <- reactive({
    
    if(input$use_demo_graph)
      inFile<- "../../DataIngest/p.pacificus_neural.synaptic_1.graphml"
    else
      inFile<-input$file1$datapath
    
    graph<- read.graph(inFile,
     format = "graphml")
    if(!is.connected(graph)){
      graph<-decompose.graph(graph)[[1]]
    }
    graph <- as.undirected(graph, mode = "collapse")
    graph
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
  
  #simplified graph with k core
  graph_kcore<- reactive({
    g<- graphFile()
    if(input$use_k_core){
      coreness <- graph.coreness(g, "all")
      verticesHavingMaxCoreness <- which(coreness >= input$par_k_cores)
      g <- induced.subgraph(graph = g, vids = verticesHavingMaxCoreness)
    }
    g
  })
  
  
  # get adjacency
  Adjacency <- reactive({
    graph <- graph_kcore()
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
  subLaplacian <- reactive({
    A <- Adjacency() 
    denseA <- as.matrix(A) * 1
    D<- rowSums(denseA)
    denseA <-  diag(D) - denseA
    denseA
  })
  
  
  # get normalized laplacian
  Laplacian <- reactive({
    A <- Adjacency() 
    denseA <- as.matrix(A) * 1
    D<- rowSums(denseA)
    D_half <- sqrt(D) 
    denseA <-  diag(1 / D_half) %*% denseA %*% diag(1 / D_half)
    denseA
  })
  
  # kmeans
  Cluster <- reactive({
    # L<- Laplacian()
    # A <- Adjacency() 

    if(input$svd_mat=="Adjacency")
      svdL <- svd(Adjacency())
    if(input$svd_mat=="Normalized Laplacian")
      svdL <- svd(Laplacian())
    if(input$svd_mat=="Laplacian")
      svdL <- svd(subLaplacian())
    
    u<- svdL$u[,1:input$embedding_d]
    d<- svdL$d[1:input$embedding_d]
    X<- u %*% diag(sqrt(d))
    clusters <- kmeans(X, input$embedding_k)
    clusters
    # group_col= clusters$cluster
    # group_col
  })
  

  
  #1st viz: layout
  output$graph_layout <- renderForceNetwork({
    graph <- graph_kcore()
    
    

    
    if (!input$use_k_core) {
      members <- rep(1, length(V(graph)))
      if(input$embedding_sbm){
        members = Cluster()$cluster
      }
      graph_d3 <- igraph_to_networkD3(graph, members)
    } else{
      coreness <- graph.coreness(graph)
      maxCoreness <- max(coreness)

      verticesHavingMaxCoreness <- which(coreness >= input$par_k_cores)
      

      
      kcore <-
      induced.subgraph(graph = graph, vids = verticesHavingMaxCoreness)
      members <- rep(1, length(V(kcore)))
      
      if(input$embedding_sbm){
        members = Cluster()$cluster
        members<- members[coreness >= input$par_k_cores]    }

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
    graph <- graph_kcore()
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
        binwidth = min_binwidth
        )
      

    }
    
    if (input$show_histogram) {
      p <- ggplot(df, aes(x)) +
      geom_histogram(
        aes(y = ..density..),
        alpha = 0.7,
        fill = "#333333",
        bins = input$hist_number_bins
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
    graph <- graph_kcore()
    A <- Adjacency()
    L <- Laplacian()

    if(input$adjacency_mode=="Adjacency"){
      denseA <- as.matrix(A) * 1
      caption <- "Adjacency: A"
      
    }
    
    if(input$adjacency_mode=="Laplacian"){
      denseA<- subLaplacian()
      caption <- "Laplacian: D-A"
      
      }
    
    if(input$adjacency_mode=="Normalized Laplacian"){
      denseA<- L
      caption <- "Normalized Laplacian: D^{-1/2}A D^{-1/2}"
      
    }
    
    if(input$adjacency_mode=="Diffusion"){
      denseA <- as.matrix(A) * 1
      denseA<- diag(1/rowSums(denseA)) %*% denseA 
      caption <- "Diffusion: D^{-1}A"
      
    }
    
    if (input$adjacency_sortedby == "Degree")   {
      orderByDegree<- order(rowSums(denseA),decreasing = T)
      denseA <- denseA[orderByDegree, orderByDegree]
    } 
    
    if (input$adjacency_sortedby == "Block" &
        input$embedding_sbm
    )   {
      
      group_col = Cluster()$cluster
      orderByBlock = order(group_col,decreasing = F)
      denseA <- denseA[orderByBlock, orderByBlock]
      
    }
    
    vals <- unique(c(scales::rescale(c(denseA,1,0))))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Blues", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)



    rownames(denseA) <- c(1:nrow(denseA))
    colnames(denseA) <- c(1:nrow(denseA))


    m<-melt(denseA)


    p <- ggplot(m, aes(X1, X2)) +
    geom_tile(aes(fill =  value), colour = "white") + 
    scale_fill_gradient(low = "white",  high = "red", limits=c(0,1)) +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank()) + labs(title = caption)

    ggplotly(p)



      # vals <- c(1,0) #unique(c(scales::rescale(c(denseA,1,0))))
      # o <- order(vals, decreasing = FALSE)
      # cols <- scales::col_numeric("Blues", domain = NULL)(vals)
      # colz <- setNames(data.frame(vals[o], cols[o]), NULL)


      # plot_ly(z = denseA,
      #   type = "heatmap",
      #   colorscale = colz)





    # plot_ly(z = denseA,
    #         type = "heatmap",
    #         colorscale = colz)


  })
  
  
  output$caption_adjacency <- renderText({ 
    
  })

  output$p_hat_view <- renderPlotly({

    if(input$embedding_sbm){
      
      if( input$embedding_model == 'Stochastic Block Model'){
        cluster<- Cluster()
        Xhat <- cluster$centers[cluster$cluster,]
        p_hat <- Xhat %*% t(Xhat)
      }

      
      if( input$embedding_model == 'Random Dot Product Model'){
        
        if(input$svd_mat=="Adjacency")
          svdL <- svd(Adjacency())
        if(input$svd_mat=="Normalized Laplacian")
          svdL <- svd(Laplacian())
        if(input$svd_mat=="Laplacian")
          svdL <- svd(subLaplacian())
        
        u<- svdL$u[,1:input$embedding_d]
        d<- svdL$d[1:input$embedding_d]
        Xhat <- u %*% diag(sqrt(d))
        
        p_hat <- Xhat %*% t(Xhat)
      }
     
      



      if (input$adjacency_sortedby == "Degree")   {
        A <- Adjacency()
        denseA <- as.matrix(A) * 1
        orderByDegree<- order(rowSums(denseA),decreasing = T)
        p_hat <- p_hat[orderByDegree, orderByDegree]
      } 
      
      
      if (input$adjacency_sortedby == "Block" &
          input$embedding_sbm
          )   {
        
        group_col = Cluster()$cluster
        orderByBlock = order(group_col,decreasing = F)
        p_hat <- p_hat[orderByBlock, orderByBlock]
        
      }

      p_hat[p_hat<0]<-0
      p_hat[p_hat>1]<-1

      
      rownames(p_hat) <- c(1:nrow(p_hat))
      colnames(p_hat) <- c(1:nrow(p_hat))

      m<-melt(p_hat)
      
      caption <- "Estimated Probability" 

      p <- ggplot(m, aes(X1, X2)) +
      geom_tile(aes(fill =  value), colour = "white") + 
      scale_fill_gradient(low = "white",  high = "red", limits=c(0,1)) +
      theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank())+ labs(title = caption)

      ggplotly(p)

    #   vals <- unique(c(scales::rescale(c(p_hat,1,0))))
    #   o <- order(vals, decreasing = FALSE)
    #   cols <- scales::col_numeric("Blues", domain = NULL)(vals)
    #   colz <- setNames(data.frame(vals[o], cols[o]), NULL)


    #   plot_ly(z = p_hat,
    #     type = "heatmap",
    #     colorscale = colz)
    }

  })

  
  #4th viz: spree plot
  output$spree_plot <-  renderPlotly({
    graph <- graph_kcore()
   
    if(input$svd_mat=="Adjacency")
      svdL <- svd(Adjacency())
    if(input$svd_mat=="Normalized Laplacian")
      svdL <- svd(Laplacian())
    if(input$svd_mat=="Laplacian")
      svdL <- svd(subLaplacian())
    
    spree_ev <- svdL$d
    
    n<- length(spree_ev)
    x<- c(1:n)
    y<- spree_ev
    z<- rep(1,n)
    
    
    if (input$elbow_detect) {
      eb1 <- opt_d(y)
      if (eb1 < n) {
        z[(eb1 + 1):n] <- 2
        eb2 <- opt_d(y[(eb1 + 1):n]) + eb1
        
        if (eb2 < n) {
          z[(eb2 + 1):n] <- 3
          eb3 <- opt_d(y[(eb2 + 1):n]) + eb2
          if (eb3 < n)
            z[(eb3 + 1):n] <- 4
        }
      }
    }
    
    df2 <- as.data.frame(cbind( x,y,z ))
    
                                    
    p<- ggplot(df2, aes(x=x, y=y, col=z)) + geom_line()
    
    ggplotly(p)
    

  })
  
  #5th viz: pairs plot
  
  
  output$max_ev_range <- renderUI({
    graph <- graph_kcore()
    A <- Adjacency()
    
    sliderInput("eigenvector_range", "View Eigenvectors (up to 9):",
      min = 1, max = nrow(A), value = c(1,5),step = 1)
  })

  output$pD3 <-  renderPairsD3({

    graph <- graph_kcore()
    A <- Adjacency()
    L <- Laplacian()
    if(input$svd_mat=="Adjacency")
      svdL <- svd(Adjacency())
    if(input$svd_mat=="Normalized Laplacian")
      svdL <- svd(Laplacian())
    if(input$svd_mat=="Laplacian")
      svdL <- svd(subLaplacian())
    
    a <- input$eigenvector_range[1]
    b <- min(input$eigenvector_range[2], input$eigenvector_range[1]+8)
    

    
    if(input$embedding_sbm){

      group_col = Cluster()$cluster
    }else{
      group_col = rep(1,nrow(A))
    }
    
    u<- svdL$u[,a:b]
    d<- svdL$d[a:b]
    X<- u %*% diag(sqrt(d))
    p<- pairsD3(X,cex = 5, group=group_col)

    
    

    # print(p)

  })
  
  output$pairsplot <- renderUI({
    pairsD3Output("pD3", 100*9,100*9)
  })
  
  

})