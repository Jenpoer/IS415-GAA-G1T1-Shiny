pacman::p_load(tidyverse, shiny, tools)

# ----------------------
# Main Data
# ----------------------
boundary_data <- list.files("data/boundaries", full.names = TRUE) %>% # list all files
  lapply(read_rds) %>%
  singleton()

names(boundary_data) <- list.files("data/boundaries") %>%
  sapply(basename) %>%
  sapply(tools::file_path_sans_ext)

hotspot_data <- list.files("data/points", full.names = TRUE) %>% # list all files
  lapply(read_rds) %>% 
  lapply(mutate, `date_str`= date, `date` = as.Date(date, format = "%d-%m-%Y")) %>%
  singleton()

names(hotspot_data) <- list.files("data/points") %>%
  sapply(basename) %>%
  sapply(tools::file_path_sans_ext) %>%
  sapply(function(x) gsub("_hotspots", "", x))

# ----------------------
# Utility Functions
# ----------------------
get_city_boundary <- function(province, city_name) {
  return(boundary_data[[province]] %>% filter(city == city_name))
}

get_district_boundary <- function(province, district_name) {
  return(boundary_data[[province]] %>% filter(district == district_name))
}

get_sub_district_boundary <- function(province, sub_district_name) {
  return(boundary_data[[province]] %>% filter(sub_district == sub_district_name))
}

get_city_hotspots <- function(province, city) {
  boundary <- get_city_boundary(province, city)
  hotspots <- hotspot_data[[province]]
  
  return(st_intersection(boundary, hotspots) %>%
           select(names(hotspots)))
}

get_district_hotspots <- function(province, district) {
  boundary <- get_district_boundary(province, district)
  hotspots <- hotspot_data[[province]]
  
  return(st_intersection(boundary, hotspots) %>%
           select(names(hotspots)))
}

get_sub_district_hotspots <- function(province, sub_district) {
  boundary <- get_district_boundary(province, sub_district)
  hotspots <- hotspot_data[[province]]
  
  return(st_intersection(boundary, hotspots) %>%
           select(names(hotspots)))
}