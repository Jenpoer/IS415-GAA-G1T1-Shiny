#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, fresh)

# Load all page files
page_files <- list.files("pages", 
           pattern="*.R", full.names=TRUE, 
           ignore.case=TRUE)
sapply(page_files, source)

# Theme
mytheme <- create_theme(
  theme = "default",
  bs_vars_color(
    brand_primary = "#e9851d"
  ),
  bs_vars_navbar(
    default_bg = "#364B45",
    default_color = "#EDECED",
    default_link_color = "#EDECED",
    default_link_active_color = "#e9851d",
    default_link_active_bg = "#142520",
    default_link_hover_color = "#e9851d",
    default_link_hover_bg = "#142520",
  ),
  bs_vars_button(
    default_bg="#ffa826",
    default_color = "#142520",
    default_border="#ffa826"
  ),
  output_file = NULL
)

# Main UI
ui <- navbarPage(
    header=use_theme(mytheme),
    title=div(tags$img(src="logo.svg", alt="logo", width=65),
              tags$style(HTML(".irs--shiny .irs-bar {background: #ffa826; border-color: #ffa826;}")),
              tags$style(HTML(".irs--shiny .irs-single, .irs--shiny .irs-to, .irs--shiny .irs-from {background: #ffa826; color: #142520}"))),
    windowTitle="FINE: Fire Incidents Explorer",
    tabPanel("Home", home_ui),
    tabPanel("Exploratory Data Analysis", eda_ui("eda_point_map", "eda_time_series")),
    tabPanel("Kernel Density", kde_ui("KDE_map")),
    tabPanel("Spatial Cluster", spatial_cluster_ui("spatial_cluster_plot")),
    tabPanel("Spatiotemporal", spatiotemporal_ui("st_mann_kendall_plot", 
                                                 "st_mann_kendall_table",
                                                 "st_ehsa_choropleth",
                                                 "st_stik_plot"))
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
  observe(kde_refresh_bw(input, session))
  
  # --------------------
  # Spatial Cluster
  # --------------------
  observe(sc_refresh_city_inputs(input, session))
  observe(sc_refresh_correction_inputs(input, session))
  output$spatial_cluster_plot <- spatial_cluster_server(input)
  output$sc_location_text <- sc_param_location_server(input)
  output$sc_date_text <- sc_param_date_server(input)
  output$sc_nrow_text <- sc_param_nrow_server(input)
  output$sc_function_text <- sc_param_function_server(input)
  output$sc_sig_level_text <- sc_param_sig_level_server(input)

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
