#load data
pacman::p_load(sf, tidyverse, tmap, maptools, raster, spatstat, sfdep, shinyjs, shinycssloaders)
source("data_manager.R")

kde_boundary <- boundary_data[["ACEH"]]
kde_hp <- hotspot_data[["ACEH"]]

# -----------------------
# Main UI
# -----------------------
kde_ui <- function(map){
  return(
    fluidPage(
      titlePanel("Kernel Density Estimate Map"),
      sidebarLayout(
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
            choices=c(unique(kde_hp$city)),
            selected= unique(kde_hp$city)[4]
          ),
          selectInput(
            "Year",
            "Year",
            choices=c("2015", "2016", "2017", "2018", "2019"),
            selected="2015"
          ),
          selectInput(
            "bwMet",
            "Bandwidth selection method",
            choices=c("Adaptive", "Fixed"),
            selected="Adaptive"
          ),
          disabled(
            selectInput(
              "kerMet",
              "Kernel method",
              choices=c("gaussian", "epanechnikov", "quartic", "disc"),
              selected="gaussian"
            )
          ),
          disabled(
            sliderInput("bwSel",
                        "bandwidth selection:",
                        min = 100, max = 1000, value = c(500)
            )
          ),
          actionButton("kde_submit", "Submit")
        ),
        mainPanel(
          withSpinner(plotOutput(map), type = 1, color="#e9851d")
        )
      ),
      hr(),
      h4("Description"),
      p("Kernel Density Estimation (KDE) is a method used to estimate the 
                  density function of a random variable from a sample of observations.
                  The basic idea behind KDE is to estimate the density at each point by placing a kernel 
                  (a symmetric, non-negative function) at that point and then summing the contributions 
                  of all kernels. The shape of the kernel function determines 
                  the shape of the estimated density function"),
      h4("Warning"),
      p("There might be an error message when switching provinces.
        Just select a city and this error should go away.
        Some cities will take a longer time to load than others.")
    
    )
  )
}


# -----------------------
# Main Server Function
# -----------------------
kde_server <- function(input){
  kde_plot_func <- eventReactive(input$kde_submit, {
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
    # kde.bw <- density(kde_ppp.km, sigma=bw.ppl, edge=TRUE, kernel="gaussian")
    
    if (input$bwMet == "Fixed"){
      kde_bw_fixed <- density(kde_ppp.km, sigma=(input$bwSel/1000), edge=TRUE, kernel=input$kerMet)
      gridded_kde_bw <- as.SpatialGridDataFrame.im(kde_bw_fixed)
    }
    else{
      kde_bw_adaptive <- adaptive.density(kde_ppp.km, method="kernel")
      gridded_kde_bw <- as.SpatialGridDataFrame.im(kde_bw_adaptive)
    }
    # gridded_kde_bw <- as.SpatialGridDataFrame.im(kde.bw)
    # gridded_kde_bw <- as.SpatialGridDataFrame.im(kde_bw_fixed)
    kde_bw_raster <- raster(gridded_kde_bw)
    projection(kde_bw_raster) <- CRS("+init=EPSG:23845 +units=km")
    kde_plot <- tm_shape(kde_bw_raster) + 
      tm_raster("v", palette = "YlOrBr", title="Hostpots") +
      tm_layout(
        legend.position = c("left", "bottom"), 
        main.title = input$kde_city,
        frame = FALSE
      )
    
    return(kde_plot)
  })
  

  return(
    kde_plot_func()
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

kde_refresh_bw <- function(input, session) {
  return({
    input$bwMet
    if(input$bwMet == "Fixed") {
      enable("kerMet")
      enable("bwSel")
    } else {
      disable("kerMet")
      disable("bwSel")
    }
  })
}
