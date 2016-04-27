# Module UI function
uiDegreeDist <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    
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
  )
}

  