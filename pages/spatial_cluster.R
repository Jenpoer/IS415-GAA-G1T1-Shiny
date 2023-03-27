# Imports
pacman::p_load(shiny, sf, tidyverse, tmap, shinyalert)
source("data_manager.R")

# Set Default Data
sc_plot_data <- hotspot_data[["aceh"]]

# UI
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
          selected="aceh"
          ),
        selectInput(
          inputId="sc_city",
          label="City",
          choices=c(unique(sc_plot_data$city)),
          selected=sc_plot_data$city[1]
        ),
        selectInput(
          inputId="sc_year",
          label="Year",
          choices = c("2015",
                      "2016",
                      "2017",
                      "2018",
                      "2019"),
          selected="2015"
        ),
        selectInput(
          inputId="sc_month",
          label="Month",
          choices = c("Jan" = "01", "Feb" = "02", "Mar" = "03", 
                      "Apr" = "04", "May" = "05", "Jun" = "06", 
                      "Jul" = "07", "Aug" = "08", "Sep" = "09",
                      "Oct" = "10", "Nov" = "11", "Dec" = "12"),
          selected="Jan"
        ),
        # function type
        # nsim
        # correction function
        ),
      mainPanel(
        withSpinner(tmapOutput(map), type=1)
      )
  )))
}

sc_refresh_city_inputs <- function(input, session) {
  return ({
    input$sc_province
    updateSelectInput(session, inputId="sc_city", label="City",
                      choices=c(unique(hotspot_data[[input$sc_province]]$city)),
                      selected=unique(hotspot_data[[input$sc_province]]$city)[1])
  })
}


spatial_cluster_server <- function(input) {
  selected_month <- paste(input$sc_month, input$sc_year, sep="-")
  
  sc_plot_data <<- hotspot_data[[input$sc_province]] %>%
    filter(city == input$sc_city) %>%
    filter(grepl(selected_month, date_str))
  
  # Create Owin
  # sc_city_boundary <- get_city_boundary(province, city_name)
  
  # Convert to ppp
  # Estimate
  # Monte Carlo
  
  if(nrow(sc_plot_data) < 1) {
    shinyalert("No Rows Found", "Please try another month!", type = "error")
  }
  
  return(
    tm_shape(sc_plot_data) +
      tm_dots() +
      tm_view(set.zoom.limits=c(8,15))
    )
}

