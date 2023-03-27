# Imports
pacman::p_load(shiny, sf, tidyverse, tmap)
source("data_manager.R")

# Set Default Data
plot_data <- hotspot_data[["aceh"]]

spatial_cluster_ui <- function(map) {
  return(div(
    titlePanel("Spatial Cluster"),
    sidebarLayout(
      sidebarPanel(
        "Selection Options",
        selectInput(
          "sc_province",
          "Province",
          choices=names(hotspot_data),
          "aceh"
          )
        ),
      mainPanel(
        withSpinner(tmapOutput(map), type=1)
      )
  )))
}

spatial_cluster_change_inputs <- function(input, session) {
  return ({
    input$province
  })
}


spatial_cluster_server <- function(input) {
  plot_data <<- hotspot_data[[input$sc_province]]
  
  # Create Owin
  # Convert to ppp
  # Estimate
  # Monte Carlo
  
  
  print(plot_data)
  
  return(
    tm_shape(plot_data) +
      tm_dots() +
      tm_view(set.zoom.limits=c(8,15))
    )
}

