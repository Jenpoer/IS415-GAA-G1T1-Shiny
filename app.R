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

# Main UI
ui <- navbarPage(
    title="FINE",
    tabPanel("Home", home_ui),
    tabPanel("Exploratory Data Analysis", eda_ui("eda_point_map")),
    tabPanel("Kernel Density", kde_ui),
    tabPanel("Spatial Cluster", spatial_cluster_ui("spatial_cluster_plot")),
    tabPanel("Spatiotemporal", spatiotemporal_ui),
    inverse=T
)

# Main Server
server <- function(input, output, session) {
  # --------------------
  # EDA
  # --------------------
  observe(eda_refresh_inputs(input, session))
  observe(eda_refresh_district(input, session))
  output$eda_point_map <- renderTmap({eda_point_map_server(input)})
  
  # --------------------
  # Spatial Cluster
  # --------------------
  observe(sc_refresh_city_inputs(input, session))
  output$spatial_cluster_plot <- renderTmap({spatial_cluster_server(input)})
}

# Run the application 
shinyApp(ui = ui, server = server)
