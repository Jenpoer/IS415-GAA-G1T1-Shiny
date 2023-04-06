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
    tabPanel("Spatiotemporal", spatiotemporal_ui("st_mann_kendall_plot", 
                                                 "st_mann_kendall_table",
                                                 "st_ehsa_choropleth",
                                                 "st_stik_plot")),
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
  output$eda_point_map <- renderTmap({eda_point_map_server(input)})
  
  # --------------------
  # KDE
  # --------------------
  output$KDE_map <- renderPlot({kde_server(input)})
  observe(kde_refresh_inputs(input, session))
  
  # --------------------
  # Spatiotemporal
  # --------------------
  observe(st_refresh_inputs(input, session))
  observe(st_refresh_district(input, session))
  observe(st_refresh_sub_district(input, session))
  observe(st_refresh_inputs_2(input, session))
  observe(st_refresh_inputs_3(input, session))
  observe(st_refresh_district_3(input, session))
  observe(st_refresh_sub_district_3(input, session))
  observe(st_refresh_persp_plot_input_3(input, session))
  st_gi_star <- st_gi_star_server(input)
  output$st_mann_kendall_plot <- st_gi_star_plot_server(st_gi_star)
  output$st_mann_kendall_table <- st_mann_kendall_server(st_gi_star)
  output$st_ehsa_choropleth <- st_ehsa_server(input)
  output$st_stik_plot <- st_stik_server(input)
}

# Run the application 
shinyApp(ui = ui, server = server)
