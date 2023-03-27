pacman::p_load(tidyverse, sf, tmap, shinycssloaders, shinyjs)
source("data_manager.R")

eda_data_current <- hotspot_data[["aceh"]]

eda_ui <- function(point_map) {
  return(
    tabsetPanel(
      tabPanel(title="Point Map", 
               eda_point_map_ui(point_map)),
      tabPanel(title="Time Series", 
               div())
    )
  )
}

eda_point_map_ui <- function(map) {
  return(
    div(
      sidebarPanel(
        useShinyjs(),
        selectInput(
          "eda_province",
          "Province",
          choices=names(hotspot_data)
        ),
        selectInput(
          "eda_city",
          "City/Regency",
          choices=c("All", unique(eda_data_current$city)),
          selected="All"
        ),
        disabled(
          selectInput(
            "eda_district",
            "District",
            choices=c("All", unique(eda_data_current$district)),
            selected="All"
          )
        ),
        disabled(
          selectInput(
            "eda_sub_district",
            "Sub-District",
            choices=c("All", unique(eda_data_current$sub_district)),
            selected="All"
          )
        ),

        sliderInput("eda_date_range",
                  "Dates:",
                  min = min(eda_data_current$date),
                  max = max(eda_data_current$date),
                  value = c(
                    min(eda_data_current$date),
                    min(eda_data_current$date) + 
                      floor((max(eda_data_current$date)-min(eda_data_current$date))/2)
                  )
        )
      ),
      mainPanel(
        withSpinner(tmapOutput(map), type=1)
      )
    )
  )
}

eda_refresh_inputs <- function(input, session) {
  return ({
    input$eda_province
    updateSelectInput(session, "eda_city",
                      choices=c("All", unique(hotspot_data[[input$eda_province]]$city)),
                      selected="All")
    updateSliderInput(session, "eda_date_range",
                      min = min(hotspot_data[[input$eda_province]]$date),
                      max = max(hotspot_data[[input$eda_province]]$date),
                      value = c(
                        min(hotspot_data[[input$eda_province]]$date),
                        min(hotspot_data[[input$eda_province]]$date) +
                          floor((max(hotspot_data[[input$eda_province]]$date)-
                                   min(hotspot_data[[input$eda_province]]$date))/2)
                      ))
  })
}

eda_refresh_district <- function(input, session) {
  return ({
    input$eda_city
    updateSelectInput(session, "eda_district",
                      choices=c("All", unique(get_city_hotspots(input$eda_province, 
                                                                input$eda_city)$district)),
                      selected="All")
    if(input$eda_city != "All") {
      enable("eda_district")
      
    } else {
      disable("eda_district")
    }
  })
}

eda_refresh_sub_district <- function(input, session) {
  return ({
    input$eda_district
    updateSelectInput(session, "eda_sub_district",
                      choices=c("All", unique(get_district_hotspots(input$eda_province, 
                                                                input$eda_district)$sub_district)),
                      selected="All")
    if(input$eda_district != "All") {
      enable("eda_sub_district")
      
    } else {
      disable("eda_sub_district")
    }
  })
}


eda_point_map_server <- function(input) {
  eda_data_current <<- hotspot_data[[input$eda_province]]
  if(input$eda_city != "All") {
    eda_data_current <<- get_city_hotspots(input$eda_province, input$eda_city)
    if(input$eda_district != "All") {
      eda_data_current <<- get_district_hotspots(input$eda_province, input$eda_district)
      if(input$eda_sub_district != "All") {
        eda_data_current <<- get_sub_district_hotspots(input$eda_province, input$eda_sub_district)
      }
    }
  }
  
  eda_data_current <- eda_data_current %>% 
    filter(`date` >= input$eda_date_range[1], 
           `date` <= input$eda_date_range[2]) %>%
    mutate(`color` = ifelse(`date` < max(date), 'pink', 'red'))
  
  if(nrow(eda_data_current) < 1) {
    return(
      tm_shape(boundary_data[[input$eda_province]]) +
        tm_borders(alpha = 0) +
        tm_view(bbox=st_bbox(boundary_data[[input$eda_province]]))
    )
  } else {
    return(
      tm_shape(eda_data_current) +
        tm_dots(col="color", alpha=0.4))
  }
  
   
}
