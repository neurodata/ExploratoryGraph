# Module UI function
uiGraphLayout <- function(id) {
  # Create a namespace function using the provided id
  # ns <- NS(id)
  
  tagList(
    forceNetworkOutput(outputId = "graph_layout", height = "500px")
  )
}

  