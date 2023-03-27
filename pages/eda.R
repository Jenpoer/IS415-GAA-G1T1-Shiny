pacman::p_load(tidyverse, sf, tmap, shinycssloaders)
source("data_manager.R")

default_hotspots <- hotspot_data[["aceh"]]

eda_ui <- function(point_map) {
  return(
    tabsetPanel(
      tabPanel(title="Point Map", 
               eda_point_map_ui(point_map))
    )
  )
}

eda_point_map_ui <- function(map) {
  return(
    div(
      sidebarPanel(
        selectInput(
          "province",
          "Province",
          choices=names(hotspot_data)
        ),
        sliderInput("DatesMerge",
                  "Dates:",
                  min = min(default_hotspots$date),
                  max = max(default_hotspots$date),
                  value = c(
                    min(default_hotspots$date),
                    min(default_hotspots$date) + 
                      floor((max(default_hotspots$date)-min(default_hotspots$date))/2)
                  )
        )
      ),
      mainPanel(
        withSpinner(tmapOutput(map), type=1)
      )
    )
  )
}

eda_change_date_slider <- function(input, session) {
  return ({
    input$province
    updateSliderInput(session, "DatesMerge", 
                      min = min(hotspot_data[[input$province]]$date),
                      max = max(hotspot_data[[input$province]]$date),
                      value = c(
                        min(hotspot_data[[input$province]]$date),
                        min(hotspot_data[[input$province]]$date) + 
                          floor((max(hotspot_data[[input$province]]$date)-
                                   min(hotspot_data[[input$province]]$date))/2)
                      ))
  })
}


eda_point_map_server <- function(input) {
  data_current <- hotspot_data[[input$province]] %>%
    filter(`date` >= input$DatesMerge[1], 
           `date` <= input$DatesMerge[2]) %>%
    mutate(`color` = ifelse(`date` < max(date), 'pink', 'red'))
  
  return(
    tm_shape(data_current) +
    tm_dots(col="color", alpha=0.4)) +
    tm_view(set.zoom.limits=c(11,15))
}
