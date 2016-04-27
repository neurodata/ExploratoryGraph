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
        label = strong("Degree Distribution"),
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
                       uiGraphLayout("layout")),
      
      conditionalPanel("input.plot_degree == true",
                       uiDegreeDist("degree")),
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
