require("shiny")
require("plotly")

# source("degreeDist.r")
# source("graphLayout.r")
# source("adjacencyPlot.r")


require("networkD3", quietly = T)

shinyUI(fluidPage(
  style = "padding-top: 80px;",
  
  absolutePanel(
    top = 20,
    right = 20,
    width = 300,
    fixed = TRUE,
    draggable = TRUE,
    
    wellPanel(
      titlePanel("GraphX"),
      
      checkboxInput("use_demo_graph", "Demo Graph",  value = TRUE),
      
      
      fileInput('file1', 'Upload Graph',
                accept = c('.graphml')),
      
      
      
      
      checkboxInput(
        inputId = "plot_degree",
        label = strong("Vertex Statistics"),
        value = FALSE
      ),
      checkboxInput(
        inputId = "plot_adjacencyy",
        label = strong("Adjacency Matrix"),
        value = TRUE
      ),
      checkboxInput(
        inputId = "plot_layout",
        label = strong("Graph Layout"),
        value = FALSE
      ),
      
      checkboxInput(
        inputId = "plot_spree",
        label = strong("Spree Plot"),
        value = FALSE
      ),
      
      checkboxInput(
        inputId = "plot_pairs",
        label = strong("Pairs Plot"),
        value = FALSE
      )
    ),
    
    
    wellPanel(
      selectInput(
        inputId = "svd_mat",
        label = "Using the spectral decomp of",
        choices = c(
          "Adjacency",
          "Normalized Laplacian",
          "Laplacian"
        ),
        selected = "Normalized Laplacian"
      ),
      
      checkboxInput(
        inputId = "embedding_sbm",
        label = strong("Embedding?"),
        value = FALSE
      ),
      conditionalPanel(
        "input.embedding_sbm == true",
        
        selectInput(
          inputId = "embedding_model",
          label = "Model",
          choices = c(
            "Random Dot Product Model",
            "Stochastic Block Model"
          ),
          selected = "Stochastic Block Model"
        ),
        
        sliderInput(
          inputId = "embedding_d",
          label = "Latent dimension (d):",
          min = 1,
          max = 50,
          ticks = T,
          step = 1,
          value = 5
        ),
        
        conditionalPanel(
          "input.embedding_model == 'Stochastic Block Model'",
        sliderInput(
          inputId = "embedding_k",
          label = "Blocks (K):",
          min = 1,
          max = 50,
          ticks = T,
          step = 1,
          value = 5
        )
        )
      ),
      
      checkboxInput(
        inputId = "use_k_core",
        label = strong("Use k-core?"),
        value = FALSE
      ),
      conditionalPanel(
        "input.use_k_core == true",
       uiOutput("KinKcore")
      )
      
    )
  ),
  
  mainPanel(
    conditionalPanel(
      "input.plot_degree == true",
      
      selectInput(
        inputId = "vertex_stats",
        label = "Vertex Statistics:",
        choices = c(
          "Degree",
          "Betweenness Centrality",
          "Closeness Centrality",
          "Eigenvector Centrality"
        ),
        selected = "Degree"
      ),
      
      plotlyOutput(outputId = "density_dist", height = "300px"),
      
      checkboxInput(
        inputId = "show_histogram",
        label = strong("Show histogram"),
        value = FALSE
      ),
      
      #show this only if we bin the observations and show it in histogram
      conditionalPanel(
        condition = "input.show_histogram == true",
        sliderInput(
          inputId = "hist_number_bins",
          label = "No. of Bins:",
          min = 1,
          max = 20,
          ticks = F,
          step = 1,
          value = 10
        )
      ),
      
      checkboxInput(
        inputId = "density",
        label = strong("Show density estimate"),
        value = FALSE
      ),
      
      # Display this only if the density is shown
      conditionalPanel(
        condition = "input.density == true",
        sliderInput(
          inputId = "bw_adjust",
          label = "Bandwidth adjustment:",
          min = 0.2,
          max = 1,
          value = .5,
          step = 0.2
        )
      )
    ),
    
    
    conditionalPanel(
      "input.plot_adjacencyy == true",
      selectInput(
        inputId = "adjacency_mode",
        label = "View:",
        choices = c("Adjacency", "Laplacian", "Normalized Laplacian", "Diffusion"),
        selected = "Adjacency"
      ),
      
      textOutput("caption_adjacency"),
      
      column(
        3,
        
        plotlyOutput(
          outputId = "adjacency_view",
          height = 500,
          width = 600
        )
      ),
      
      
      column(
        3,
        offset = 3,
        
        conditionalPanel(
          "input.embedding_sbm == true",
          plotlyOutput(
            outputId = "p_hat_view",
            height = 500,
            width = 600
          )
        )
      ),
      
      selectInput(
        inputId = "adjacency_sortedby",
        label = "Sorted By:",
        choices = c("Unsorted", "Degree", "Block"),
        selected = "Unsorted"
      )
      
      
    ),
    
    
    
    conditionalPanel(
      "input.plot_layout == true",
      forceNetworkOutput(outputId = "graph_layout", height = "500px")
    ),
    
    #spree plot
    conditionalPanel(
      "input.plot_spree == true",
      # numericInput("spectral_d_to_view", "Number of eigenvalues to keep:", 5),
      plotlyOutput(outputId = "spree_plot", height = 300),
      checkboxInput(
        inputId = "elbow_detect",
        label = strong("Elbow Detection"),
        value = FALSE
      )
    ),
    
    
    #pairs plot
    conditionalPanel(
      "input.plot_pairs == true",
      uiOutput("max_ev_range"),
      uiOutput("pairsplot")
    )
  )
))
# )
