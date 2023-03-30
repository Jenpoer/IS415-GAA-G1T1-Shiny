pacman::p_load(tidyverse, sf, tmap, shinycssloaders, shinyjs, shinyalert)
source("data_manager.R")

eda_data_current <- hotspot_data[["aceh"]]

eda_ui <- function(point_map, time_series) {
  return(
    tabsetPanel(
      tabPanel(title="Point Map", 
               eda_point_map_ui(point_map)),
      tabPanel(title="Time Series", 
               eda_time_series_ui(time_series))
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
        selectInput(
          "eda_satellite",
          "Satellite",
          choices=c("TERRA/AQUA",
                    "SNPP",
                    "NOAA20",
                    "NASA-MODIS",
                    "NASA-SNPP",
                    "NASA-NOAA20"),
          multiple = T
        ),
        checkboxInput(
          "eda_color_by_confidence",
          "Color by confidence level?"
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

eda_time_series_ui <- function(time_series) {
  return(
    div(
      sidebarPanel(
        useShinyjs(),
        selectInput(
          "eda_province_2",
          "Province",
          choices=names(hotspot_data)
        ),
        selectInput(
          "eda_city_2",
          "City/Regency",
          choices=c("All", unique(eda_data_current$city)),
          selected="All"
        ),
        disabled(
          selectInput(
            "eda_district_2",
            "District",
            choices=c("All", unique(eda_data_current$district)),
            selected="All"
          )
        ),
        disabled(
          selectInput(
            "eda_sub_district_2",
            "Sub-District",
            choices=c("All", unique(eda_data_current$sub_district)),
            selected="All"
          )
        ),
        selectInput(
          "eda_year",
          "Year",
          choices=c("2015",
                    "2016",
                    "2017",
                    "2018",
                    "2019"),
          selected="2015"
        ),
        selectInput(
          "eda_confidence",
          "Confidence Level",
          choices=c("Low",
                    "Medium",
                    "High"),
          multiple=T
        )
      ),
      mainPanel(
        withSpinner(plotOutput(time_series), type=1)
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

eda_refresh_inputs_2 <- function(input, session) {
  input$eda_province_2
  updateSelectInput(session, "eda_city_2",
                    choices=c("All", unique(hotspot_data[[input$eda_province_2]]$city)),
                    selected="All")
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

eda_refresh_district_2 <- function(input, session) {
  return ({
    input$eda_city_2
    updateSelectInput(session, "eda_district_2",
                      choices=c("All", unique(get_city_hotspots(input$eda_province_2, 
                                                                input$eda_city_2)$district)),
                      selected="All")
    if(input$eda_city_2 != "All") {
      enable("eda_district_2")
      
    } else {
      disable("eda_district_2")
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

eda_refresh_sub_district_2 <- function(input, session) {
  return ({
    input$eda_district
    updateSelectInput(session, "eda_sub_district_2",
                      choices=c("All", unique(get_district_hotspots(input$eda_province_2, 
                                                                    input$eda_district_2)$sub_district)),
                      selected="All")
    if(input$eda_district_2 != "All") {
      enable("eda_sub_district_2")
      
    } else {
      disable("eda_sub_district_2")
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
  
  eda_data_current <<- eda_data_current %>% 
    filter(`date` >= input$eda_date_range[1], 
           `date` <= input$eda_date_range[2]) %>%
    mutate(`color` = ifelse(`date` < max(date), 'pink', 'red'),
           `confidence` = factor(`confidence`, levels=c("Low", "Medium", "High")))
  
  if(length(input$eda_satellite) > 0) {
    eda_data_current <<- eda_data_current %>% filter(satellite %in% input$eda_satellite)
  }
  
  if(nrow(eda_data_current) < 1) {
    shinyalert("Something's Burning", "No records for the selected parameters.", type = "error")
    stop()
  } else {
    if(input$eda_color_by_confidence) {
      return(
        tm_shape(eda_data_current) +
          tm_dots(palette="RdYlGn", col="confidence", alpha=0.4))
    } else {
      return(
        tm_shape(eda_data_current) +
          tm_dots(col="color", alpha=0.4))
    }
    
  }
}

eda_time_series_server <- function(input) {
  eda_data_current <<- hotspot_data[[input$eda_province_2]]
  if(input$eda_city_2 != "All") {
    eda_data_current <<- get_city_hotspots(input$eda_province_2, input$eda_city_2)
    if(input$eda_district_2 != "All") {
      eda_data_current <<- get_district_hotspots(input$eda_province_2, input$eda_district_2)
      if(input$eda_sub_district_2 != "All") {
        eda_data_current <<- get_sub_district_hotspots(input$eda_province_2, input$eda_sub_district_2)
      }
    }
  }
  
  eda_data_current <<- eda_data_current %>%
    filter(grepl(input$eda_year, date_str)) %>%
    st_drop_geometry()
  
  if(length(input$eda_confidence) > 0) {
    eda_data_current <<- eda_data_current %>%
      mutate(`confidence` = factor(`confidence`, levels=c("Low", "Medium", "High"))) %>%
      filter(confidence %in% input$eda_confidence) %>%
      group_by(date, confidence) %>%
      summarise(count=n())
    
    return(ggplot(eda_data_current, 
                  aes(x=date, y=count, group=confidence, color=confidence)) +
             geom_line() + 
             xlab(""))
  } else {
    eda_data_current <<- eda_data_current %>%
      group_by(date) %>%
      summarise(count=n())
    
    return(ggplot(eda_data_current, 
                  aes(x=date, y=count)) +
             geom_line() + 
             xlab(""))
  }
    
}
