# Imports
pacman::p_load(shiny, sf, tidyverse, tmap)

# Load Data
aceh_boundary <- read_rds("data/boundaries/aceh.rds")

spatial_cluster_ui <- function(map) {
  return(div(
    titlePanel("Spatial Cluster"),
    sidebarLayout(
      sidebarPanel("Side bar panel"),
      mainPanel(
        tmapOutput(map)
      ))))}

spatial_cluster_server <- function(input) {
  return(qtm(aceh_boundary))
}