pacman::p_load(tidyverse, shiny, tools)

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