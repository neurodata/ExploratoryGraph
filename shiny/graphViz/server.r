require("shiny")
require("plotly")
require("igraph", quietly = T)
require("networkD3", quietly = T)
source("opt_d.r")
require("pairsD3")
require("plyr")
require("reshape")
source("igraph2D3.r")

shinyServer(function(input, output) {

  # Read in the graph from graphml file
  graphFile <- reactive({

    if(input$use_demo_graph)
      inFile<- "./demo2.graphml"
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
    if(input$use_k_core){
      sliderInput(
        inputId = "par_k_cores",
        label = "K:",
        min = 1,
        max = maxCoreness,
        ticks = T,
        step = 1,
        value = 10
        )
    }
  })
  
  #simplified graph with k core
  graph_kcore<- reactive({
    g<- graphFile()
    if(input$use_k_core){
      coreness <- graph.coreness(g, "all")
      verticesHavingMaxCoreness <- which(coreness >=  
        input$par_k_cores)
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

    # if(input$svd_mat=="Adjacency")
    svdL <- svd(Adjacency())
    # if(input$svd_mat=="Normalized Laplacian")
      # svdL <- svd(Laplacian())
    # if(input$svd_mat=="Laplacian")
      # svdL <- svd(subLaplacian())
    
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
    
    
    members <- rep(1, length(V(graph)))

    if(input$embedding_sbm  & input$embedding_model == 'Stochastic Block Model'){
      members = Cluster()$cluster
        # members<- members[coreness >= input$par_k_cores]    
    }

    graph_d3 <- igraph2D3(graph, members)

    forceNetwork(
      Links = graph_d3$links,
      Nodes = graph_d3$nodes,
      Source = 'source',
      Target = 'target',
      NodeID = 'name',
      Group = 'group',
      zoom = TRUE
      )
  })

  # # vertex stats
  # output$vertex_dist <- renderPlotly({

  #   graph <- graph_kcore()
  #   dg <- degree(graph)

  #   block<- rep(1, length(dg))

  #   if(      input$embedding_sbm){
  #   block <- Cluster()$cluster
  #   }

  #   df <- data.frame(x <- dg, block <- factor(block))
  #   names(df)<- c("x","block")


  #   p <- ggplot(df, aes(x=x, fill=block)) + geom_density(alpha = 0.7, bw = input$bw_adjust_overlay)+   ggtitle("Degree in each Block")

  #   # geom_histogram(
  #   #  position="identity",
  #   #   aes(y = ..density..),
  #   #     alpha = 0.7,
  #   #     binwidth = 1
  #   #   )  
  #   ggplotly(p)


  # })

  output$AttributeSelect <- renderUI({
    graph <- graphFile()
    vertexAtt<- get.vertex.attribute(graph)
    count<- lapply(vertexAtt, function(x){length(unique(x))})
    nameAtt<- names(vertexAtt)
    pickAtt<- nameAtt[(count<10 & count>=2)]

    selectInput(
        inputId = "pickAttribute",
        label = "Pick attributes",
        choices =  pickAtt ,
        selected = pickAtt[1]
        )
  })


    # attributes condtional stats
  output$vertex_dist <- renderPlotly({

    graph <- graph_kcore()

     if(input$graph_stats=="Degree"){
        bw <- degree(graph)
      }
      if(input$graph_stats=="Betweenness Centrality"){
        bw <- betweenness(graph)

      }
      if(input$graph_stats=="Closeness Centrality"){
        bw <- closeness(graph)

      }
      if(input$graph_stats=="Eigenvector Centrality"){
        eg<- evcent(graph)
        bw <- eg$vector

      }

      if(input$graph_stats=="Bonacich Power Centrality"){
        bw<- power_centrality(graph)

      }
      
      if(input$graph_stats=="Authority Score"){
        bw<- authority_score(graph)$vector
      }


    Attribute<- rep("overall", length(V(graph)))
    if(input$vertex_conditional){

      vertexAtt<- get.vertex.attribute(graph)

      Attribute <- vertexAtt[[input$pickAttribute]]

      countAttribute<- table(Attribute)

      namesWithCountSmallerThan5<- names(countAttribute[countAttribute <=5])

      Attribute[Attribute %in% namesWithCountSmallerThan5]<- paste("Others: ", paste(namesWithCountSmallerThan5, collapse = ',') , collapse = " ")
    }


    df <- data.frame(x <- bw, Attribute <- factor(Attribute))
    names(df)<- c("x","attribute")


    p <- ggplot(df, aes(x=x, fill=Attribute)) + geom_density(alpha = 0.7, bw = input$bw_adjust_overlay)+   ggtitle(input$graph_stats)

    # geom_histogram(
    #  position="identity",
    #   aes(y = ..density..),
    #     alpha = 0.7,
    #     binwidth = 1
    #   )  
    ggplotly(p)


  })

  #2nd viz:graph stats
  output$density_dist <- renderPlotly({
    # g<- graphFile()
    graph <- graph_kcore()
    A <- Adjacency()

    if (input$plot_degree) {
      if(input$graph_stats=="Degree"){
        bw <- degree(graph)
        df <- data.frame(x <- bw, group <- 1)
        min_binwidth = 1
      }
      if(input$graph_stats=="Betweenness Centrality"){
        bw <- betweenness(graph)
        df <- data.frame(x <- bw, group <- 1)
        min_binwidth = (quantile(bw,0.6)-quantile(bw,0.4))/20
      }
      if(input$graph_stats=="Closeness Centrality"){
        bw <- closeness(graph)
        df <- data.frame(x <- bw, group <- 1)
        min_binwidth = (quantile(bw,0.9)-quantile(bw,0.1))/10
      }
      if(input$graph_stats=="Eigenvector Centrality"){
        eg<- evcent(graph)
        bw <- eg$vector
        df <- data.frame(x <- bw, group <- 1)
        min_binwidth = (quantile(bw,0.9)-quantile(bw,0.1))/10
      }

      if(input$graph_stats=="Bonacich Power Centrality"){
        bw<- power_centrality(graph)
        df <- data.frame(x <- bw, group <- 1)
        min_binwidth = (quantile(bw,0.9)-quantile(bw,0.1))/10
      }
      
      if(input$graph_stats=="Authority Score"){
        bw<- authority_score(graph)$vector
        df <- data.frame(x <- bw, group <- 1)
        min_binwidth = (quantile(bw,0.9)-quantile(bw,0.1))/10
      }

      # if(input$graph_stats=="Attribute: Edge Weight"){
      #   bw<- edge_attr(graph)$weight
      #   df <- data.frame(x <- bw, group <- 1)
      #   min_binwidth = (quantile(bw,0.9)-quantile(bw,0.1))/10
      # }

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
       bw = input$bw_adjust)
    }
    
    if (input$plot_degree) {
      p <- p +
      theme(panel.background = element_rect(fill = '#ffffff')) +
      ggtitle(input$graph_stats)
      
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
      # caption <- "Adjacency: A"
      
    }
    
    if(input$adjacency_mode=="Laplacian"){
      denseA<- subLaplacian()
      # caption <- "Laplacian: D-A"
      
    }
    
    if(input$adjacency_mode=="Normalized Laplacian"){
      denseA<- L
      caption <- expression(D^(-1/2)*A *D^(-1/2))
      
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

    zlim <- c(0,1)

    if(input$adjacency_mode=="Laplacian")
      zlim<- c(min(denseA),max(denseA))


    p <- ggplot(m, aes(X1, X2)) +
    geom_tile(aes(fill =  value), colour = "white") + 
    scale_fill_gradient(low = "white",  high = "red", limits=zlim) +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank()) 
    # + labs(title = paste(caption) )


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
  
  
  output$caption_adjacency <- renderUI({ 

    if (input$adjacency_mode == "Adjacency")   {
      caption = 'Adjacency: \\(A\\)'
    }

    if (input$adjacency_mode == "Laplacian")   {
      caption = 'Laplacian: \\(D-A\\)'
    }

    if (input$adjacency_mode == "Normalized Laplacian")   {
      caption = 'Normalized Laplacian: \\(D^{1/2}AD^{1/2}\\)'
    }

    if (input$adjacency_mode == "Diffusion")   {
      caption = 'Diffusion: \\(D^{-1}A\\)'
    }



    withMathJax(
      helpText(caption)
      )

  })

  output$p_hat_view <- renderPlotly({

    if(input$embedding_sbm){

      if( input$embedding_model == 'Stochastic Block Model'){
        cluster<- Cluster()
        Xhat <- cluster$centers[cluster$cluster,]
        p_hat <- Xhat %*% t(Xhat)
      }

      
      if( input$embedding_model == 'Random Dot Product Model'){

        # if(input$svd_mat=="Adjacency")
        svdL <- svd(Adjacency())
        # if(input$svd_mat=="Normalized Laplacian")
          # svdL <- svd(Laplacian())
        # if(input$svd_mat=="Laplacian")
          # svdL <- svd(subLaplacian())
        
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

  
  #4th viz: scree plot
  output$scree_plot <-  renderPlotly({
    graph <- graph_kcore()


    p<- ggplot() 

    n<- length(V(graph))
    x<- c(1:n)


    df2 <- data.frame( index= x)


    if(input$scree_A){
      svdL <- svd(Adjacency())

      y <- svdL$d  
      y<- y/y[1]
      df2$Adjacency = y
    }

    if(input$scree_L){
      svdL <- svd(subLaplacian())

      y <- svdL$d  
      y<- y/y[1]
      df2$Laplacian = y

    }

    if(input$scree_nL){
      svdL <- svd(Laplacian())

      y <- svdL$d  
      y<- y/y[1]
      df2$nL = y
    }

    df3 = melt(df2, id=c("index"))


    
    
    p<- ggplot(df3) + geom_line(aes(x=index, y=value, colour=variable),linetype =1)    + scale_colour_manual(values = c('Adjacency' = "red", 'Laplacian' = "green", 'nL' = "blue"))

    if (input$elbow_detect) {

      if(input$scree_A){

        y<- df2$Adjacency
        y<- y/y[1]

        eb1 <- opt_d(y)

        if (eb1 < n) {
          eb2 <- opt_d(y[(eb1 + 1):n]) + eb1
          if (eb2 < n) {
            eb3 <- opt_d(y[(eb2 + 1):n]) + eb2
          }
        }
        p<- p+  geom_vline(xintercept = c(eb1,eb2,eb3), linetype =3, col="red")
        #+ geom_vline(xintercept = eb2, linetype = 3, col="red")  + geom_vline(xintercept = eb3, linetype =3, col="red")
      }

      


      
      if(input$scree_L){

        y<- df2$Laplacian
        y<- y/y[1]

        eb1 <- opt_d(y)

        if (eb1 < n) {
          eb2 <- opt_d(y[(eb1 + 1):n]) + eb1
          if (eb2 < n) {
            eb3 <- opt_d(y[(eb2 + 1):n]) + eb2
          }
        }
        p<- p+  geom_vline(xintercept = c(eb1,eb2,eb3), linetype =3, col="green")
      }


      if(input$scree_nL){
        y<- df2$nL
        y<- y/y[1]


        eb1 <- opt_d(y)

        if (eb1 < n) {
          eb2 <- opt_d(y[(eb1 + 1):n]) + eb1
          if (eb2 < n) {
            eb3 <- opt_d(y[(eb2 + 1):n]) + eb2
          }
        }
        p<- p+  geom_vline(xintercept = c(eb1,eb2,eb3), linetype =3, col="blue")
      }

    }  
    
    ggplotly(p)
    

  })
  
  #5th viz: pairs plot
  
  output$pD3 <-  renderPairsD3({

    graph <- graphFile()
    A <- Adjacency()
    # L <- Laplacian()
    # if(input$svd_mat=="Adjacency")
    svdL <- svd(A)
    # if(input$svd_mat=="Normalized Laplacian")
      # svdL <- svd(Laplacian())
    # if(input$svd_mat=="Laplacian")
      # svdL <- svd(subLaplacian())
    
    a <- input$eigenvector_range[1]
    b <- min(input$eigenvector_range[2], input$eigenvector_range[1]+8)
    

    
    if(input$embedding_sbm  & input$embedding_model == 'Stochastic Block Model'){
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
    pairsD3Output("pD3", width = '700px', height = '700px')
  })
  
  

})