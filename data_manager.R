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

#
# Functions for getting data by administrative division
#
get_city_boundary <- function(province, city_name) {
  return(boundary_data[[province]] %>% filter(city == city_name))
}

get_district_boundary <- function(province, district_name) {
  return(boundary_data[[province]] %>% filter(district == district_name))
}

get_sub_district_boundary <- function(province, sub_district_name) {
  return(boundary_data[[province]] %>% filter(sub_district == sub_district_name))
}

get_city_hotspots <- function(province, city_name) {
  hotspots <- hotspot_data[[province]] %>% 
    filter(city == city_name)
  
  return(hotspots)
}

get_district_hotspots <- function(province, district_name) {
  hotspots <- hotspot_data[[province]] %>% 
    filter(district == district_name)
  
  return(hotspots)
}

get_sub_district_hotspots <- function(province, sub_district_name) {
  hotspots <- hotspot_data[[province]] %>%
    filter(sub_district == sub_district_name)
  
  return(hotspots)
}

#
# Functions for converting data into Spatstat objects
#

convert_polygon_to_owin <- function(st_polygon_df) {
  print("Polygons: -------------")
  print(st_polygon_df)
  owin <- st_polygon_df %>%
    as_Spatial() %>%
    as("SpatialPolygons") %>%
    as("owin")
  print("Owin: -------------")
  print(owin)
  return(owin)
}

convert_points_to_ppp <- function(st_points_df) {
  ppp <- st_points_df %>%
    as_Spatial() %>%
    as("SpatialPoints") %>%
    as("ppp")
  
  return(ppp)
}

convert_to_spatstat <- function(st_polygon_df, st_points_df) {
  owin <- convert_polygon_to_owin(st_polygon_df)
  ppp <- convert_points_to_ppp(st_points_df)
  
  return(ppp[owin])
}

# How to call the function:
# Province level: convert_to_spatstat(boundary_data[[province]], hotspot_data[[province]])
# Everything else: convert_to_spatstat(get_city_boundary(province, city), get_city_hotspots(province, city))
