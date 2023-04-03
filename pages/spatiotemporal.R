pacman::p_load(tidyverse, sf, tmap, shinycssloaders, shinyjs, sfdep, stpp, shinyalert, grDevices)
source("data_manager.R")

st_data_current <- hotspot_data[["aceh"]]

# Main UI

spatiotemporal_ui <- function(mk_plot, mk_table, ehsa_map, stik_plot) {
  return(
    tabsetPanel(
      tabPanel(title="Mann Kendall", 
               st_mann_kendall_ui(mk_plot, mk_table)),
      tabPanel(title="Emerging Hotspot Analysis", 
               st_ehsa_ui(ehsa_map)),
      tabPanel(title="Space-Time K-Function", 
               st_stik_ui(stik_plot))
    )
  )
}

##########################
# UI
##########################

st_mann_kendall_ui <- function(plot, table) {
  return(
    div(
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
        withSpinner(plotOutput(plot), type=1),
        withSpinner(tableOutput(table), type=1)
      )
    )
  )
}

st_ehsa_ui <- function(map) {
  return(
    div(
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
        withSpinner(plotOutput(map), type=1)
      )
    )
  )
}

st_stik_ui <- function(plot_name) {
  return(
    div(
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
        withSpinner(plotOutput(plot_name), type=1),
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
    )
  )
}

##########################
# Refresh Inputs 1
##########################

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

##########################
# Refresh Inputs 2
##########################

st_refresh_inputs_2 <- function(input, session) {
  return ({
    input$st_province_2
    updateSelectInput(session, "st_city_2",
                      choices=c("", unique(hotspot_data[[input$st_province_2]]$city)),
                      selected="")
  })
}

##########################
# Refresh Inputs 3
##########################
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

##########################
# Utility Server Functions
##########################
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

##########################
# Mann Kendall Output
##########################
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

##########################
# EHSA Output
##########################
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
  
##########################
# STIK Output
##########################
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