require("shiny")
require("plotly")

source("degreeDist.r")
source("graphLayout.r")
source("adjacencyPlot.r")


require("networkD3", quietly = T)

shinyUI(fluidPage(
  titlePanel("Explorer"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      checkboxInput(
        inputId = "plot_layout",
        label = strong("Graph Layout"),
        value = FALSE
      ),
      checkboxInput(
        inputId = "plot_degree",
        label = strong("Vertex Statistics"),
        value = FALSE
      ),
      checkboxInput(
        inputId = "plot_adjacencyy",
        label = strong("Adjacency Matrix"),
        value = FALSE
      ),
      checkboxInput(
        inputId = "embedding",
        label = strong("Spectral Embedding"),
        value = FALSE
      )
      
    ),
    mainPanel(
      conditionalPanel("input.plot_layout == true",
                       forceNetworkOutput(outputId = "graph_layout", height = "500px"),
                       checkboxInput(
                         inputId = "use_k_core",
                         label = strong("Use k-core?"),
                         value = FALSE
                       ),
                
                       uiOutput("KinKcore")
                       
                      
                       ),
      
      conditionalPanel(
        "input.plot_degree == true",
        
        selectInput(
          inputId = "vertex_stats",
          label = "Vertex Statistics:",
          choices = c("Degree", "Betweenness Centrality","Closeness Centrality", "Eigenvector Centrality"),
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
            inputId = "binwidth",
            label = "Bin width:",
            min = 1,
            max = 50,
            ticks = T,
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
        plotlyOutput(outputId = "adjacency_view", height = 500),
        selectInput(
          inputId = "adjacency_sortedby",
          label = "Sorted By:",
          choices = c("Unsorted", "Degree"),
          selected = "Unsorted"
        )
      ),
      conditionalPanel(
        "input.embedding == true",
        plotlyOutput(outputId = "spree_plot", height = 300),
        h1(textOutput("optimal_d")),
        numericInput("spectral_d_to_view", "Number of eigenvalues to keep:", 5),
        plotlyOutput(outputId = "embeded_scatter", height = 500),
        numericInput("cluster_K", "Cluster Number (K)", 3)
      )
    )
  )
))
