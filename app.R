#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny)

# Load all page files
page_files <- list.files("pages", 
           pattern="*.R", full.names=TRUE, 
           ignore.case=TRUE)
sapply(page_files, source)

# 
ui <- navbarPage(
    title="FINE",
    tabPanel("Home", home_ui),
    tabPanel("Exploratory Data Analysis", eda_point_map_ui("eda_point_map")),
    tabPanel("Kernel Density", kde_ui),
    tabPanel("Spatial Cluster", spatial_cluster_ui("spatial_cluster_plot")),
    tabPanel("Spatiotemporal", spatiotemporal_ui),
    inverse=T
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$eda_point_map <- renderTmap({eda_point_map_server(input)})
  output$spatial_cluster_plot <- renderTmap({spatial_cluster_server(input)})
}

# Run the application 
shinyApp(ui = ui, server = server)
