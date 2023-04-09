pacman::p_load(tidyverse, sf, tmap, shinycssloaders, shinyjs, sfdep, stpp, shinyalert, zoo)
source("data_manager.R")

st_data_current <- hotspot_data[["ACEH"]]

# -----------------------
# Main UI
# -----------------------

spatiotemporal_ui <- function(mk_plot, mk_table, ehsa_map, stik_plot) {
  return(
    fluidPage(
      titlePanel("Spatiotemporal Analysis"),
      tabsetPanel(
        tabPanel(title="Space-Time K-Function", 
                 st_stik_ui(stik_plot)),
        tabPanel(title="Emerging Hotspot Analysis", 
                 st_ehsa_ui(ehsa_map)),
        tabPanel(title="Mann Kendall", 
                 st_mann_kendall_ui(mk_plot, mk_table)),
      )
    )
  )
}

# -----------------------
# UI
# -----------------------

st_mann_kendall_ui <- function(plot, table) {
  return(
    div(
      sidebarLayout(
        sidebarPanel(
          useShinyjs(),
          selectInput(
            "st_province",
            "Province",
            choices=names(hotspot_data)
          ),
          selectInput(
            "st_city",
            "City/Regency",
            choices=c("", unique(st_data_current$city))
          ),
          disabled(
            selectInput(
              "st_district",
              "District",
              choices=c("", unique(st_data_current$district))
            )
          ),
          disabled(
            selectInput(
              "st_sub_district",
              "Sub-District",
              choices=c("", unique(st_data_current$sub_district))
            )
          ),
          numericInput(
            "st_mk_nsim",
            "Number of simulations",
            value = 39
          ),
          selectInput(
            "st_mk_contiguity_weights",
            "Type of Contiguity Weights",
            choices=c("Row Standardised", "Inverse Distance")
          ),
          selectInput(
            "st_year",
            "Year",
            choices=c("2015",
                      "2016",
                      "2017",
                      "2018",
                      "2019")
          ),
          actionButton("st_submit", "Submit")
        ),
        mainPanel(
          withSpinner(plotOutput(plot), type=1, color="#e9851d"),
          withSpinner(tableOutput(table), type=1, color="#e9851d"),
        ),
      ),
      hr(),
      div(
        h4("Description"),
        p("The Mann-Kendall test is a non-parametric statistical test used to detect whether a set of data values is monotonically increasing or decreasing, as well as the statistical significance of the trend. It is particularly useful when the data is not normally distributed or when the data has a significant amount of noise."),
        p("In this section, you can perform Mann-Kendall tests on computed Getis-Ord Gi* statistics within a sub-district in a specified year. This will aid in closely examining the changes in the hot spots / cold spots over a year, and statistically confirm the presence of any trends."),
        h4("How it works"),
        p("For every test we want to perform, we define the hypothesis as such:"),
        tags$ul(
          tags$li("H0: There is no monotonic trend in the series."),
          tags$li("H1:  A trend exists. This trend can be positive, negative, or non-null.")
        ),
        p(HTML(paste0("The Mann Kendall test is performed using the Kendall package. For more information, please refer to the ", 
                      a(href = 'https://cran.r-project.org/web/packages/Kendall/Kendall.pdf', 
                        "official documentation"), "."))),
        p("After running the test and observing a p-value, you can choose whether to reject the null hypothesis or not based on your own chosen significance level."),
        h4("Intepretation"),
        p("After running the function with all your selected parameters, you will see a time series plot of the change in Gi* values in the sub-district over the chosen year."),
        p("Below it, you'll see the results of the Mann-Kendall test in a table. The values in the table are:"),
        tags$ul(
          tags$li("tau: Kendall's tau statistic"),
          tags$li("sl: Two-sided p-value"),
          tags$li("S: Kendall Score"),
          tags$li("D: Denominator (tau = S / D)"),
          tags$li("varS: Variance of S")
        ),
        p("Kendall's tau statistic varies between -1 and 1. A positive number indicates an upwards trend, whereas a negative number indicates a downwards trend."),
        p("The two-sided p-value will help you determine whether or not it is appropriate to reject the null hypothesis.")
      )
    )
    
  )
}

st_ehsa_ui <- function(map) {
  return(
    div(
      sidebarLayout(
        sidebarPanel(
          useShinyjs(),
          selectInput(
            "st_province_2",
            "Province",
            choices=names(hotspot_data)
          ),
          selectInput(
            "st_city_2",
            "City/Regency",
            choices=c("", unique(st_data_current$city))
          ),
          numericInput(
            "st_ehsa_k",
            "Number of time lags",
            value = 1
          ),
          numericInput(
            "st_ehsa_nsim",
            "Number of simulations",
            value = 99
          ),
          numericInput(
            "st_ehsa_threshold",
            "Significance threshold",
            value = 0.01
          ),
          selectInput(
            "st_year_2",
            "Year",
            choices=c("2015",
                      "2016",
                      "2017",
                      "2018",
                      "2019")
          ),
          actionButton("st_submit_2", "Submit")
        ),
        mainPanel(
          withSpinner(plotOutput(map), type=1, color="#e9851d")
        )
      ),
      hr(),
      div(
        h4("Description"),
        p("Emerging Hot Spot Analysis (EHSA) is a spatiotemporal analysis technique used to identify locations where the density of a specific event is increasing rapidly over time."),
        p("Note that 'hot spot' in this context does not refer to fire hotspots, but spatial clusters with significantly high number of fire hotspots."),
        p("In this section, you can perform EHSA on an areal basis using aggregated count of fire hotspots within a province or a city. I.e. the feature we are looking at is the count of fire hotspots in a region."),
        h4("How it works"),
        p("The EHSA is performed using sfdep's emerging_hotspots_analysis, which combines the Getis-Ord Gi* statistic with Mann-Kendall test."),
        p("The Gi* statistic calculates the local spatial autocorrelation for each feature in a dataset by comparing the feature's value with the values of neighboring features."),
        p("You are able to adjust the number of time lags, number of simulations for Gi* calculation, and significance threshold."),
        p(HTML(paste0("For more information, please refer to the ", 
                      a(href = 'https://sfdep.josiahparry.com/reference/emerging_hotspot_analysis.html', 
                        "official documentation"), "."))),
        h4("Intepretation"),
        p("After running the function with all your selected parameters, you will see a choropleth map. The different colors of the sub-districts indicate the kind of trend is detected with statistical significance."),
        p("The possible patterns are:"),
        tags$ul(
          tags$li("No pattern detected"),
          tags$li("New Hot Spot / Cold Spot: A location that is a statistically significant hot spot (or cold spot) at the final time step without having been significant prior."),
          tags$li("Consecutive Hot Spot / Cold Spot: A location with a single uninterrupted run of at least 2 statistically significant hot spot (or cold spot) bins in the final time-step intervals."),
          tags$li("Intensifying Hot Spot / Cold Spot: A location that has been a statistically significant hot spot (or cold spot) for 90% of the time and the intensity of clustering increases."),
          tags$li("Persistent Hot Spot / Cold Spot: A location that has been a statistically significant hot spot (or cold spot) for 90% of the time-step intervals with no trend in the clustering intensity."),
          tags$li("Diminishing Hot Spot / Cold Spot: A location that has been a statistically significant hot spot (or cold spot) for 90% of the time-step intervals and the intensity of clustering decreases."),
          tags$li("Sporadic Hot Spot / Cold Spot: A statistically significant hot spot (or cold spot) for the final time step interval with a history of being an on-off hot spot (or cold spot). No history of being a statistically significant cold spot (or hot spot)."),
          tags$li("Oscillating Hot Spot / Cold Spot: A statistically significant hot spot (or cold spot) for the final time step interval with a history of being an on-off hot spot (or cold spot). Has a history of being a statistically significant cold spot (or hot spot)."),
          tags$li("Historical Hot Spot / Cold Spot: The most recent time period is not hot (or cold), but at least 90% of time step intervals have been statistically significant hot spots (or cold spots).")
        ),
      )
    )
  )
}

st_stik_ui <- function(plot_name) {
  return(
    div(
      sidebarLayout(
        sidebarPanel(
          useShinyjs(),
          selectInput(
            "st_province_3",
            "Province",
            choices=names(hotspot_data)
          ),
          selectInput(
            "st_city_3",
            "City/Regency",
            choices=c("", unique(st_data_current$city))
          ),
          disabled(
            selectInput(
              "st_district_3",
              "District",
              choices=c("", unique(st_data_current$district))
            )
          ),
          disabled(
            selectInput(
              "st_sub_district_3",
              "Sub-District",
              choices=c("", unique(st_data_current$sub_district))
            )
          ),
          selectInput(
            "st_year_3",
            "Year",
            choices=c("2015",
                      "2016",
                      "2017",
                      "2018",
                      "2019")
          ),
          actionButton("st_submit_3", "Submit")
        ),
        mainPanel(
          withSpinner(plotOutput(plot_name), type=1, color="#e9851d"),
          fluidRow(
            useShinyjs(),
            column(4, 
                   selectInput(
                     "st_stik_plot_type",
                     "Plot Type",
                     choices=c("Contour",
                               "Image",
                               "Perspective")
                   )
            ),
            column(4, 
                   disabled(sliderInput(
                     "st_persp_theta",
                     "Azimuthal Angle",
                     min = 0,
                     max = 360,
                     value = 0
                   ))
            ),
            column(4, 
                   disabled(sliderInput(
                     "st_persp_phi",
                     "Colatitude",
                     min = 0,
                     max = 360,
                     value = 15
                   ))
            ))
        )
      ),
      hr(),
      div(
        h4("Description"),
        p("The space-time K-function is a statistical tool used in spatiotemporal analysis to study the clustering or dispersion of point events in both space and time. It extends the classical spatial Ripley's K-function to incorporate the time dimension and allows for the analysis of spatiotemporal point patterns."),
        p("In the inhomogenous case, it takes into account the spatial and temporal variation in the intensity of the point process under study. Meanwhile, the homogenous case assumes that the underlying process generating the point pattern is constant in space and time."),
        p("It provides a powerful tool for understanding the complex interactions between spatial and temporal processes and for identifying patterns and trends that might otherwise be missed."),
        h4("How it works"),
        p("You can input a specific sub-district to look for spatiotemporal autocorrelation throughout a specific year. The space-time K-function will then be calculated using stpp's STIK function for the specified parameters."),
        p(HTML(paste0("For more information, please refer to the ", 
                      a(href = 'https://rdrr.io/github/stpp-GitHub/stpp/man/STIKhat.html', 
                        "official documentation"), "."))),
        h4("Intepretation"),
        p("After running the function with all your selected parameters, you will see either a contour plot, an image plot, or a perspective plot depending on your selection of Plot Type. The axes indicate distances in meters and times in days. The plot displays values for K - 2 pi u^2 v."),
        p("Hence, a positive value implies K > 2 pi u^2 v, indicating clustering. Conversely, a negative value implies K < 2 pi u^2 v, indicating regularity.")
      )
    )
    
  )
}

# -----------------------
# Refresh Inputs 1
# -----------------------

st_refresh_inputs <- function(input, session) {
  return ({
    input$st_province
    updateSelectInput(session, "st_city",
                      choices=c("", unique(hotspot_data[[input$st_province]]$city)),
                      selected="")
  })
}

st_refresh_district <- function(input, session) {
  return ({
    input$st_city
    updateSelectInput(session, "st_district",
                      choices=c("", unique(get_city_hotspots(input$st_province, 
                                                                input$st_city)$district)),
                      selected="")
    if(input$st_city != "") {
      enable("st_district")
      
    } else {
      disable("st_district")
    }
  })
}

st_refresh_sub_district <- function(input, session) {
  return ({
    input$st_district
    updateSelectInput(session, "st_sub_district",
                      choices=c("", unique(get_district_hotspots(input$st_province, 
                                                                    input$st_district)$sub_district)),
                      selected="")
    if(input$st_district != "") {
      enable("st_sub_district")
      
    } else {
      disable("st_sub_district")
    }
  })
}

# -----------------------
# Refresh Inputs 2
# -----------------------

st_refresh_inputs_2 <- function(input, session) {
  return ({
    input$st_province_2
    updateSelectInput(session, "st_city_2",
                      choices=c("", unique(hotspot_data[[input$st_province_2]]$city)),
                      selected="")
  })
}

# -----------------------
# Refresh Inputs 3
# -----------------------
st_refresh_inputs_3 <- function(input, session) {
  return ({
    input$st_province_3
    updateSelectInput(session, "st_city_3",
                      choices=c("", unique(hotspot_data[[input$st_province_3]]$city)),
                      selected="")
  })
}

st_refresh_district_3 <- function(input, session) {
  return ({
    input$st_city_3
    updateSelectInput(session, "st_district_3",
                      choices=c("", unique(get_city_hotspots(input$st_province_3, 
                                                             input$st_city_3)$district)),
                      selected="")
    if(input$st_city_3 != "") {
      enable("st_district_3")
      
    } else {
      disable("st_district_3")
    }
  })
}

st_refresh_sub_district_3 <- function(input, session) {
  return ({
    input$st_district_3
    updateSelectInput(session, "st_sub_district_3",
                      choices=c("", unique(get_district_hotspots(input$st_province_3, 
                                                                 input$st_district_3)$sub_district)),
                      selected="")
    if(input$st_district_3 != "") {
      enable("st_sub_district_3")
      
    } else {
      disable("st_sub_district_3")
    }
  })
}

st_refresh_persp_plot_input_3 <- function(input, session) {
  return ({
    input$st_stik_plot_type
    if(input$st_stik_plot_type == "Perspective") {
      enable("st_persp_theta")
      enable("st_persp_phi")
    } else {
      disable("st_persp_theta")
      disable("st_persp_phi")
    }
  })
}

# -----------------------
# Utility Server Functions
# -----------------------
st_spacetime_server <- function(province, city, year) {
  if(city != "") {
    st_data_current <<- get_city_hotspots(province, city) %>%
      filter(grepl(year, date))
  } else {
    st_data_current <<- hotspot_data[[province]] %>%
      filter(grepl(year, date))
  }
  
  aggregated_hotspots <<- st_data_current %>%
    st_drop_geometry() %>%
    mutate(location = paste(city, district, sub_district, sep="-")) %>%
    group_by(location, date) %>%
    summarise(count=n()) %>%
    mutate(month = as.character(format(date, "%m"))) %>%
    dplyr::select(c("location", "month", "count")) %>%
    pivot_wider(id_cols="location", 
                names_expand=T, 
                names_from="month", 
                values_from="count",
                values_fn=sum) %>%
    mutate(
      across(everything(), ~replace_na(.x, 0))
    ) %>%
    pivot_longer(cols=!location, names_to="month", values_to="count")
  
  if(city != "") {
    boundary_aggregate <<- get_city_boundary(province, city)
  }
  else {
    boundary_aggregate <<- boundary_data[[province]]
  }
  
  boundary_aggregate <<- boundary_aggregate %>%
    mutate(location = paste(city, district, sub_district, sep="-")) %>%
    left_join(aggregated_hotspots, by="location", multiple="all") %>%
    drop_na()
  
  
  spacetime_cube <- boundary_aggregate %>%
    as_spacetime(.loc_col="location", .time_col="month")
  
  print(is_spacetime_cube(spacetime_cube))
  
  return(spacetime_cube)
}

st_gi_star_server <- function(input) {
  st_gi_star <- eventReactive(input$st_submit, {
    validate(
      need(input$st_district != "", "Please select a district"),
      need(input$st_sub_district != "", "Please select a sub-district")
    )
    
    spacetime_cube <- st_spacetime_server(input$st_province,
                                          input$st_city,
                                          input$st_year)

    df_nb <- spacetime_cube %>%
      activate("geometry") %>% # activate geometry context
      mutate(nb = include_self(st_contiguity(geometry)),
             wt = 
               case_when(
                 input$st_mk_contiguity_weights == "Row Standardised" ~ st_weights(nb, style = "W"),
                 input$st_mk_contiguity_weights == "Inverse Distance" ~  st_inverse_distance(nb, geometry,
                                                                                             scale = 1,
                                                                                             alpha = 1)
              ),
             .before = 1) %>%  # create neighbour and weight column
      set_wts("wt") %>%
      set_nbs("nb")

    EHSA_gi_star <- df_nb %>%
      group_by(`month`) %>%
      mutate(gi_star = local_gstar_perm(
        `count`, nb, wt, nsim=input$st_mk_nsim),
        .before = 1) %>%
      unnest(gi_star) %>%
      filter(city == input$st_city,
             district == input$st_district,
             sub_district == input$st_sub_district)

    return(EHSA_gi_star)
  })
  
  return(st_gi_star)
}

# -----------------------
# Mann Kendall Output
# -----------------------
st_gi_star_plot_server <- function(gi_star_df) {
  return (renderPlot({
  ggplot(data = gi_star_df() %>% 
           ungroup(),
         aes(x = month,
             y = gi_star,
             group = 1)) +
    geom_line() +
    theme_light()
  }))
}

st_mann_kendall_server <- function(gi_star_df) {
  return(renderTable(
    gi_star_df() %>%
    ungroup() %>%
    summarise(mk = list(
      unclass(
        Kendall::MannKendall(gi_star)))) %>% 
    tidyr::unnest_wider(mk)
  ))
}

# -----------------------
# EHSA Output
# -----------------------
st_ehsa_server <- function(input) {
  st_ehsa <- eventReactive(input$st_submit_2, {
    
    spacetime_cube <- st_spacetime_server(input$st_province_2,
                                          input$st_city_2,
                                          input$st_year_2)
    
    if(!is_spacetime_cube(spacetime_cube)) {
      shinyalert("Something's Burning", 
                 "Unaable to perform analysis on this data. Please select other parameters.", 
                 type = "error")
      stop()
    }
    
    ehsa <- emerging_hotspot_analysis(
      x = spacetime_cube,
      .var = "count",
      k = input$st_ehsa_k,
      nsim = input$st_ehsa_nsim,
      threshold = input$st_ehsa_threshold
    )
    
    if(input$st_city_2 != "") {
      sf_ehsa <- get_city_boundary(input$st_province_2,
                                   input$st_city_2) %>%
        mutate(location = paste(city, district, sub_district, sep="-")) %>%
        left_join(ehsa, by="location", multiple="all")
    }
    else {
      sf_ehsa <- boundary_data[[input$st_province_2]] %>%
        mutate(location = paste(city, district, sub_district, sep="-")) %>%
        left_join(ehsa, by="location", multiple="all")
    }
    
    return(sf_ehsa)
  })
  
  return(renderPlot({
    tm_shape(st_ehsa()) +
           tm_fill("classification") +
           tm_borders(alpha=0.5)
    }))
}
  
# -----------------------
# STIK Output
# -----------------------
st_stik_server <- function(input) {
  st_stik <- eventReactive(input$st_submit_3, {
    validate(
      need(input$st_city_3 != "", "Please select a city"),
      need(input$st_district_3 != "", "Please select a district"),
      need(input$st_sub_district_3 != "", "Please select a sub-district")
    )
    st_data_current <<- get_sub_district_hotspots(input$st_province_3,
                                          input$st_sub_district_3) %>%
      filter(grepl(input$st_year_3, date))
    
    st_3d_points <- st_data_current %>%
      cbind(st_coordinates(.)) %>%
      as_tibble() %>%
      dplyr::select(`X`, `Y`,`date`) %>%
      rename(`t` = `date`,
             `x` = `X`,
             `y` = `Y`) %>%
      as.3dpoints()
    
    boundary <- get_sub_district_boundary(input$st_province_3, input$st_sub_district_3)
    
    st_region_matrix <- st_coordinates(boundary)[,c("X","Y")]
    
    if(is.null(dim(st_3d_points))) {
      shinyalert("Something's Burning", "Please select another sub-district.", type = "error")
      stop()
    }
    
    stik <- STIKhat(xyt=st_3d_points, 
                    s.region = st_region_matrix)
    
    return(stik)
  })
  
  return(renderPlot({
    K <- st_stik()
    if(input$st_stik_plot_type == "Contour") {
      st_plot_K_contour(K)
    } else if(input$st_stik_plot_type == "Perspective") {
      st_plot_K_persp(K, theta=input$st_persp_theta, phi=input$st_persp_phi)
    } else if(input$st_stik_plot_type == "Image") {
      st_plot_K_image(K)
    }
  }))
}

# ADAPTED FROM stpp's plotK's source code

# Plot contour
st_plot_K_contour <- function(K, n=15) {
  par(cex.lab=1.5,cex.axis=1.5,font=2,plt=c(0,1,0,1),lwd=1,mar=c(0.5,0.5,2.5,0.5),las=1)
  k <- K$Khat - K$Ktheo
  M <- max(abs(range(k)))
  M <- pretty(c(-M,M),n=n)
  colo <- colorRampPalette(c("red", "white", "blue"))
  par(fig=c(0.1,0.825,0.1,1))
  contour(K$dist, K$times, k, labcex=1.5,levels=M,drawlabels=F,col=colo(n),zlim=range(M),axes=F)
  box(lwd=2)

  at <- axTicks(1)
  axis(1,at=at[1:length(at)],labels=at[1:length(at)])
  at <- axTicks(2)
  axis(2,at=at[1:length(at)],labels=at[1:length(at)])
  
  titl <- expression(hat(K)[ST] * group("(",list(u,v),")") - 2*pi*u^2*v) 
  title(titl,cex.main=1.5,cex.lab=1, outer=TRUE,line=-1, xlab="Distances (m)", ylab="Times")

  mini <- findInterval(x=min(k,na.rm=TRUE),vec=M)
  maxi <- findInterval(x=max(k,na.rm=TRUE),vec=M)
  
  par(fig=c(0,1,0.1,1))
  legend("right",fill=colo(n)[maxi:mini],legend=M[maxi:mini],horiz=F,bty="n")
}

st_plot_K_persp <- function(K, n=15, theta=0, phi=15) {
  k <- K$Khat - K$Ktheo
  M <- max(abs(range(k)))
  M <- pretty(c(-M,M),n=n)
  colo <- colorRampPalette(c("red", "white", "blue"))
  COL <- colo(n)
  
  mask <- matrix(0,ncol=length(K$times),nrow=length(K$dist))
  for(i in 1:length(K$dist)){ for(j in 1:length(K$times)){mask[i,j] <- COL[findInterval(x=k[i,j],vec=M)]}}
  COL <- mask[1:(length(K$dist)-1),1:(length(K$times)-1)]
  
  par(cex.lab=2,cex.axis=1.5,font=2,lwd=1,mar=c(0,0,3,0))
  par(fig=c(0,0.825,0,1))
  persp(x=K$dist, y=K$times, z=k, xlab="u",ylab="v", zlab="",expand=1, col=COL, theta = theta, phi = phi)
  
  titl <- expression(hat(K)[ST] * group("(",list(u,v),")") - 2*pi*u^2*v) 
  title(titl,cex.main=1.5,outer=TRUE,line=-1)
  
  par(fig=c(0.825,1,0,1))
  mini <- findInterval(x=min(k,na.rm=TRUE),vec=M)
  maxi <- findInterval(x=max(k,na.rm=TRUE),vec=M)
  legend("right",fill=colo(n)[maxi:mini],legend=M[maxi:mini],horiz=F,bty="n")
}

st_plot_K_image <- function(K, n=15) {
  k <- K$Khat - K$Ktheo
  M <- max(abs(range(k)))
  M <- pretty(c(-M,M),n=n)
  colo <- colorRampPalette(c("red", "white", "blue"))
  COL <- colo(n)
  
  par(cex.lab=1.5,cex.axis=1.5,font=2,lwd=1,plt=c(0,1,0,1),mar=c(0.5,0.5,2.5,0.5),las=1)
  par(fig=c(0.1,0.825,0.1,1))
  image(K$dist, K$times, k, col=colo(n),zlim=range(M),axes=F,xlab="",ylab="")
  box(lwd=2)
  
  at <- axTicks(1)
  axis(1,at=at[1:length(at)],labels=at[1:length(at)])
  at <- axTicks(2)
  axis(2,at=at[1:length(at)],labels=at[1:length(at)])
  
  titl <- expression(hat(K)[ST] * group("(",list(u,v),")") - 2*pi*u^2*v) 
  title(titl,cex.main=1.5,outer=TRUE,line=-1)
  
  par(fig=c(0,1,0.1,1))
  mini <- findInterval(x=min(k,na.rm=TRUE),vec=M)
  maxi <- findInterval(x=max(k,na.rm=TRUE),vec=M)
  legend("right",fill=colo(n)[maxi:mini],legend=M[maxi:mini],horiz=F,bty="n")
}