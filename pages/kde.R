#load data
pacman::p_load(sf, tidyverse, tmap, maptools, raster, spatstat, sfdep)
kde_boundary <- boundary_data[["aceh"]]
kde_hp <- hotspot_data[["aceh"]]


#selecting the city

source("data_manager.R")


kde_ui <- function(map){
  return(
    div(
      sidebarPanel(
        useShinyjs(),
        selectInput(
          "kde_province",
          "Province",
          choices=names(hotspot_data)
          ),
        selectInput(
          "kde_city",
          "City/Regency",
          choices=c("All", unique(kde_hp$city)),
          selected= unique(kde_hp$city)[4]
        ),
        selectInput(
          "Year",
          "Year",
          choices=c("2015", "2016", "2017", "2018", "2019"),
          selected="2015"
        ),
        ),
      
      mainPanel(withSpinner(plotOutput(map)), type = 1,
                h1("Kernel density estimation"),
                t("Kernel Density Estimation (KDE) is a method used to estimate the 
                  density function of a random variable from a sample of observations.
                  The basic idea behind KDE is to estimate the density at each point by placing a kernel 
                  (a symmetric, non-negative function) at that point and then summing the contributions 
                  of all kernels. The shape of the kernel function determines 
                  the shape of the estimated density function"),
                h1("Warning"),
                t("There will be an error msg when switching provinces.
                  Just select a city and this error should go away.
                  Some cities will take a longer time to load than others.
                  "))
                
      )
    
    )
  
  
  
  
  
}





kde_server <- function(input){
  kde_boundary <<- boundary_data[[input$kde_province]]
  kde_hp <<- hotspot_data[[input$kde_province]]
  
  kde_boundary <- kde_boundary %>% filter(city == input$kde_city) %>% st_transform(crs = 23845) #Select only the city that we are observing

  
  selected_year <- paste(input$Year)
  
  kde_hp <<- kde_hp %>%
    filter(grepl(selected_year, date_str))%>%
    drop_na()
  
  kde_ppp <- convert_points_to_ppp(kde_hp)
  kde_owin <- convert_polygon_to_owin(kde_boundary)
  kde_ppp <- kde_ppp[kde_owin]
  kde_ppp.km <- rescale(kde_ppp, 1000, "km")
  bw <- bw.ppl(kde_ppp)
  kde.bw <- density(kde_ppp.km, sigma=bw.ppl, edge=TRUE, kernel="gaussian")
  gridded_kde_bw <- as.SpatialGridDataFrame.im(kde.bw)
  kde_bw_raster <- raster(gridded_kde_bw)
  projection(kde_bw_raster) <- CRS("+init=EPSG:23845 +units=km")
  kde_plot <- tm_shape(kde_bw_raster) + 
    tm_raster("v", palette = "YlOrBr", title="Hostpots") +
    tm_layout(
      legend.position = c("left", "bottom"), 
      main.title = input$kde_city,
      frame = FALSE
    )
  kde_plot +
    tm_shape(kde_boundary) +
    tm_borders() +
    tm_text("KDE", size = 0.6)

  return(
    kde_plot
  )
}

#refresh the inputs in UI for kde city
kde_refresh_inputs <- function(input, session) {
  return ({
    input$kde_province
    updateSelectInput(session, "kde_city",
                      choices=c("All", unique(hotspot_data[[input$kde_province]]$city)),
                      selected=unique(kde_hp$city)[2])
  })
}
