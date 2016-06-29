require("shinydashboard")
require("plotly")

# source("degreeDist.r")
# source("graphLayout.r")
# source("adjacencyPlot.r")

require("networkD3", quietly = T)

# shinyUI(fluidPage(
# style = "padding-top: 80px;",



header <- dashboardHeader(title = "GraphX")


body <- dashboardBody(fluidRow(


  column(
    width = 9,
    
    #Vertex Distribution
    box(
      width = NULL,
      solidHeader = TRUE,
      conditionalPanel(
        "input.plot_degree == true",
        
        selectInput(
          inputId = "graph_stats",
          label = "Statistics:",
          choices = c(
            "Degree",
            "Betweenness Centrality",
            "Closeness Centrality",
            "Eigenvector Centrality",
            "Bonacich Power Centrality",
            "Authority Score"
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
        )
      ),
    ##end vertex distribution
    
    # adjacency view
    box(
      width = NULL,
      solidHeader = TRUE,
      

      conditionalPanel(
        "input.plot_adjacencyy == true",
        selectInput(
          inputId = "adjacency_mode",
          label = "View:",
          choices = c("Adjacency", "Laplacian", "Normalized Laplacian", "Diffusion"),
          selected = "Adjacency"
          ),
        
        
        uiOutput("caption_adjacency"),
        
        fluidRow(
          splitLayout(cellWidths = c("50%", "50%"),

            plotlyOutput(
              outputId = "adjacency_view",
              height = 500,
              width = 600
              
              ),
            
            conditionalPanel(
              "input.embedding_sbm == true",
              plotlyOutput(
                outputId = "p_hat_view",
                height = 500,
                width = 600
                )
              )
            )
          )
        ,
        
        selectInput(
          inputId = "adjacency_sortedby",
          label = "Sorted By:",
          choices = c("Unsorted", "Degree", "Block"),
          selected = "Unsorted"
          )
        
        
        )
      ),
    
    # end adjacency view
    
    # layout
    
    box(
      width = NULL,
      solidHeader = TRUE,
      conditionalPanel(
        "input.plot_layout == true",
        forceNetworkOutput(outputId = "graph_layout", height = "500px")
        )
      ),
    # end layout
    
    
    # scree plot
    
    box(
      width = NULL,
      solidHeader = TRUE,
      
      #scree plot
      conditionalPanel(
        "input.plot_scree == true",
        # numericInput("spectral_d_to_view", "Number of eigenvalues to keep:", 5),
        plotlyOutput(outputId = "scree_plot", height = 300),
        checkboxInput(
          inputId = "elbow_detect",
          label = strong("Elbow Detection"),
          value = FALSE
          )
        )
      ),
    
    #end scree plot
    #pairs plot
    box(
      width = NULL,
      solidHeader = TRUE
      ,
      
      conditionalPanel("input.plot_pairs == true",
       uiOutput("pairsplot"))
      )
    #end pairs plot
    ),

  column(
    width = 3,
    box(
      width = NULL,
      
      
      checkboxInput("use_demo_graph", "Demo Graph",  value = TRUE),
      
      
      fileInput('file1', 'Upload Graph',
        accept = c('.graphml')),
      
      
      
      checkboxInput(
        inputId = "plot_degree",
        label = strong("Graph Statistics"),
        value = FALSE
        ),
      checkboxInput(
        inputId = "plot_adjacencyy",
        label = strong("Adjacency Matrix"),
        value = FALSE
        ),
      checkboxInput(
        inputId = "plot_layout",
        label = strong("Graph Layout"),
        value = FALSE
        ),
      
      checkboxInput(
        inputId = "plot_scree",
        label = strong("Scree Plot"),
        value = TRUE
        ),


      conditionalPanel(
        "input.plot_scree == true",
        wellPanel(
          p("Show:"),
          checkboxInput(
            inputId = "scree_A",
            label = strong("Adjacency"),
            value = TRUE
            ),
          checkboxInput(
            inputId = "scree_L",
            label = strong("Laplacian"),
            value = TRUE
            ),
          checkboxInput(
            inputId = "scree_nL",
            label = strong("Normalized Laplacian"),
            value = TRUE
            )
          )
        ),
      
      checkboxInput(
        inputId = "plot_pairs",
        label = strong("Pairs Plot"),
        value = FALSE
        ),
      
      conditionalPanel(
        "input.plot_pairs == true",
        sliderInput(
          "eigenvector_range",
          "View Eigenvectors (up to 9):",
          min = 1,
          max = 9#nrow(A)
          ,
          value = c(1, 5),
          step = 1
          )
        # uiOutput("max_ev_range")
        )),



      # selectInput(
      #   inputId = "svd_mat",
      #   label = "Using the spectral decomp of",
      #   choices = c("Adjacency",
      #     "Normalized Laplacian",
      #     "Laplacian"),
      #   selected = "Normalized Laplacian"
      #   ),

    box(
      width = NULL,

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
          choices = c("Random Dot Product Model",
            "Stochastic Block Model"),
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
      
      conditionalPanel("input.use_k_core == true",
       uiOutput("KinKcore"))
      
      #     sliderInput(
      # inputId = "par_k_cores",
      # label = "K:",
      # min = 1,
      # max = 10,
      # ticks = T,
      # step = 1,
      # value = 10
      # )
      
      
      
      )
    )
  
  
  
  
  
  ))


dashboardPage(header,
  dashboardSidebar(disable = TRUE),
  body)
