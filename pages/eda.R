pacman::p_load(tidyverse, sf, tmap)

eda_ui <- function(point_map) {
  return(
    tabsetPanel(
      tabPanel(title="Point Map", 
               eda_point_map_ui(point_map))
    )
  )
}

riau <- read_rds("data/boundaries/riau.rds")
riau_hotspots <- read_rds("data/points/riau_hotspots.rds")

eda_point_map_ui <- function(map) {
  return(div(
  sidebarPanel(
    sliderInput("DatesMerge",
              "Dates:",
              min = min(as.Date(riau_hotspots$date, format = "%d-%m-%Y")),
              max = max(as.Date(riau_hotspots$date, format = "%d-%m-%Y")),
              value = median(as.Date(riau_hotspots$date, format = "%d-%m-%Y")))
    ),
  mainPanel(
    tmapOutput(map)
  )
  ))
}


eda_point_map_server <- function(input) {
  data_current <- riau_hotspots %>% 
    mutate(`date` = as.Date(date, format = "%d-%m-%Y")) %>%
    filter(`date` <= input$DatesMerge) %>%
    mutate(`color` = ifelse(`date` < max(date), 'pink', 'red'))
  
  return(tm_shape(data_current) +
    tm_dots(col="color", alpha=0.4))
}
