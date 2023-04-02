pacman::p_load(tidyverse, sf, tmap, shinycssloaders, shinyjs, sfdep)
source("data_manager.R")

st_data_current <- hotspot_data[["aceh"]]

# Main UI

spatiotemporal_ui <- function(mk_plot, mk_table, ehsa_map) {
  return(
    tabsetPanel(
      tabPanel(title="Mann Kendall", 
               st_mann_kendall_ui(mk_plot, mk_table)),
      tabPanel(title="Emerging Hotspot Analysis", 
               st_ehsa_ui(ehsa_map))
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
             wt = st_inverse_distance(nb, geometry,
                                      scale = 1,
                                      alpha = 1),
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
  
  
