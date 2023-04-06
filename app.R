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
    tabPanel("Exploratory Data Analysis", eda_ui("eda_point_map", "eda_time_series")),
    tabPanel("Kernel Density", kde_ui("KDE_map")),
    tabPanel("Spatial Cluster", spatial_cluster_ui),
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
  observe(eda_refresh_sub_district(input, session))
  observe(eda_refresh_inputs_2(input, session))
  observe(eda_refresh_district_2(input, session))
  observe(eda_refresh_sub_district_2(input, session))
  output$eda_point_map <- renderTmap({eda_point_map_server(input)})
  output$eda_time_series <- renderPlot({eda_time_series_server(input)})
  
  # --------------------
  # KDE
  # --------------------
  output$KDE_map <- renderPlot({kde_server(input)})
  observe(kde_refresh_inputs(input, session))
}

# Run the application 
shinyApp(ui = ui, server = server)