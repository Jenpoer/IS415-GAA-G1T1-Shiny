# Imports
pacman::p_load(shiny, maptools, sf, raster, spatstat, tmap, shinyalert)
source("data_manager.R")

# Set Default Data
sc_plot_data <- hotspot_data[["aceh"]]

# Constants
F_correction_options <- list("none", "rs", "km", "cs", "best", "all")
G_correction_options <- list("none", "rs", "km", "Hanisch", "best", "all")
K_L_correction_options <- list("none", "border", "bord.modif", "isotropic", 
                               "Ripley", "translate", "translation", "rigid",
                               "none", "good", "best", "all")

# -----------------------
# Main UI
# -----------------------
spatial_cluster_ui <- function(plot) {
  return(div(
    titlePanel("Statistical Test for Spatial Clustering"),
    sidebarLayout(
      sidebarPanel(
        "Selection Options",
        selectInput(
          inputId="sc_province",
          label="Province",
          choices=names(hotspot_data),
          selected="aceh"
          ),
        selectInput(
          inputId="sc_city",
          label="City",
          choices=c(unique(sc_plot_data$city)),
          selected=sc_plot_data$city[1]
        ),
        selectInput(
          inputId="sc_year",
          label="Year",
          choices = c("2015",
                      "2016",
                      "2017",
                      "2018",
                      "2019"),
          selected="2015"
        ),
        selectInput(
          inputId="sc_month",
          label="Month",
          choices = c("Jan" = "01", "Feb" = "02", "Mar" = "03", 
                      "Apr" = "04", "May" = "05", "Jun" = "06", 
                      "Jul" = "07", "Aug" = "08", "Sep" = "09",
                      "Oct" = "10", "Nov" = "11", "Dec" = "12"),
          selected="Jan"
        ),
        "Statistical Test Options",
        # function type
        selectInput(
          inputId="sc_function_type",
          label="Function Type",
          choices = c("F Function" = "Fest", "G Function" = "Gest",
                      "K Function" = "Kest", "L Function" = "Lest"),
          selected="F Function"
        ),
        # alpha = 2/(1 + nsim)
        sliderInput(inputId = "sc_nsim",
                    label = "Number of Simulations",
                    min = 19, # 0.1
                    max = 199, # 0.01
                    value = 39, # 0.05
                    step = 10),
        # correction function
        selectInput(
          inputId="sc_correction_method",
          label="Correction Method",
          choices = c(F_correction_options),
          selected="none"
        ),
        actionButton("sc_submit", "Submit")
      ),
      mainPanel(
        sc_params_summary(),
        withSpinner(plotOutput(plot), type=1),
        tabsetPanel(type = "tabs",
                    tabPanel("Overview", sc_overview()),
                    tabPanel("Inputs", sc_inputs_desc()),
                    tabPanel("F Function", sc_f_func_desc()),
                    tabPanel("G Function", sc_g_func_desc()),
                    tabPanel("K Function", sc_k_func_desc()),
                    tabPanel("L Function", sc_l_func_desc())
        )
      )
  )))
}

# -----------------------
# Static UI
# -----------------------
sc_params_summary <- function() {
  return(div(style = "border-style: solid; border-color: black; border-radius: 5px; padding-left:8px; padding-right:8px;", 
             h4("Complete Spatial Randomness Test Parameters"),
             p(textOutput("sc_location_text")),
             p(textOutput("sc_date_text")),
             p(textOutput("sc_nrow_text")),
             p(textOutput("sc_function_text")),
             p(textOutput("sc_sig_level_text"))
  ))
}

sc_overview <- function() {
  return(div(
    h4("Description"),
    p("In this section, we will be performing a statistical test for Complete 
      Spatial Randomness. This test is a second order spatial point pattern 
      analysis, as it aims to examine how individual points impact the occurance 
      of other points, and what spatial point patterns emerge as a result. In 
      particular, we will be looking out for random, clustered or dispersed point 
      patterns for occurances of fire hospots within the selected city during 
      the specifed month."),
    h4("How it Works"),
    p("When carrying out our statistical test, we must first create a hypothesis 
      to be tested. As such, let us form the hypotheses:"),
    p("H0, the Null Hypothesis: The distribution of forest fires in this region 
      is random.",align = "center"),
    p("H1, the Alternative Hypothesis: The distribution of forest fires in this 
      region is not random.",align = "center"),
    p("We will then use the selected function to obtain our estimate from our observed
      data, as represented by the solid line in the plot."),
    p("In order to determine if this observed value is statistically significant, we 
      will make use of ther Monte Carlo simulation test. This function will perform
      the selected nsim number of independent simulation of the selected data. The 
      maximum and minimum values from this simulation will then be used as the confidence 
      interval for the selected significance level, represented on our plot by the grey 
      envelope."),
    h4("Interpretation"),
    p("When the observed value lies outside the grey envelope, we can conclude 
      that the observed data is statistically significant at the stated stated 
      significance level. We can therefore conclude that the points have some 
      form of interaction with one another, and the spatial clustering
      pattern is not random. For specific interpretation on whether the statistically
      significant result follows a clustered pattern or dispersed pattern, please 
      refer to the individual function tabs.")
  ))   
}

sc_inputs_desc <- function() {
  return(div(
    h4("Province"),
    p("Province that the selected City belongs to. Changing this option will also 
      update the City options."),
    h4("City"),
    p("City to select the fire hotspot data from."),
    h4("Year"),
    p("Year to select fire hotspot data from."),
    h4("Month"),
    p("Month of the corresponding year to select fire hotspot data from."),
    h4("Function Type"),
    p("Function to be used for statistical test."),
    h4("Number of Simulations"),
    p("Number of simulations used in Monte Carlo Simulation. Affects the significant level, 
      calulated via: alpha = 2/(1 + num_simulations)"),
    h4("Correction Method"),
    p("Edge correction to be applied to the function in order to reduce bias.")
  ))
}

sc_f_func_desc <- function() {
  return(div(
    h4("Description"),
    p(HTML("The F Function, also known as the Empty Space function, measures the distribution 
      of  distances from an <b>arbitrary location</b> (not necessarily a point) to its nearest 
      observed point. ")),
    p(HTML(paste0("For more information, please refer to the ", 
      a(href = 'https://www.rdocumentation.org/packages/spatstat/versions/1.64-1/topics/Fest', 
      "official documentation"), "."))),
    h4("Interpretation"),
    p("If the observed F Function follows a concave upwards curve (increases slowly 
      at first, then more rapidly at longer distances), we can say that the observed 
      fires follow a clustered pattern. However, if the observed F function follows 
      a concave downwards curve (increases rapidly at first, then more slowly at longer 
      distances), we can say that the observed fires follow a dispersed pattern."),
    p('(Please refer to "Overview" tab for interpretation of Statistical Test Result)')
  ))
}

sc_g_func_desc <- function() {
  return(div(
    h4("Description"),
    p(HTML("The G Function measures the distribution of distances from an <b>arbitrary point</b> 
      to its nearest neighbour.")),
    p(HTML(paste0("For more information, please refer to the ", 
                  a(href = 'https://www.rdocumentation.org/packages/spatstat/versions/1.64-1/topics/Gest', 
                    "official documentation"), "."))),
    h4("Interpretation"),
    p("When the observed G Function value is greater than the estimate (lies 
      above the theoretical), we can say that the observed fires follow a clustered 
      pattern. Generally, this lies below the theoretical estimate. On the other hand, 
      when the observed value is less than the estimate, we can say the observed fires 
      follow a dispersed pattern. Generally, this lies above the theoretical estimate."),
    p('(Please refer to "Overview" tab for interpretation of Statistical Test Result)')
  ))
}

sc_k_func_desc <- function() {
  return(div(
    h4("Description"),
    p("Ripley's K Function attempts to combat the limitation of the nearest neighbours 
      approach of using only the nearest distance by using an estimate of spatial dependence 
      instead. This is done by taking a measure of how many events lie about each points 
      at varying radius length, and computing this calculation for all point events."),
    p(HTML(paste0("For more information, please refer to the ", 
                  a(href = 'https://www.rdocumentation.org/packages/spatstat/versions/1.64-1/topics/Kest', 
                    "official documentation"), "."))),
    h4("Interpretation of Statistical Test Result"),
    p("When the observed K Function value is greater than, or lies above the theotetical, we can say 
      that the forest fire hotspots are clustered and this radius. However, when the 
      observer value lies below the theotetical value, we can assume that the forest fires 
      are dispersed at this radius."),
    p('(Please refer to "Overview" tab for interpretation of Statistical Test Result)')
  ))
}

sc_l_func_desc <- function() {
  return(div(
    h4("Description"),
    p("The L function is the normalised version of the K function, where we take 
      the K function divided by pi, the squared rooted to get the corresponding L function value."),
    p(HTML(paste0("For more information, please refer to the ", 
                  a(href = 'https://www.rdocumentation.org/packages/spatstat/versions/1.64-1/topics/Lest', 
                    "official documentation"), "."))),
    h4("Interpretation"),
    p("When the L value lies above the theoretical value, the we can say that the forest 
      fire hotspots for this region are spatially clustered at that particular distance. 
      On the other hand, when the L value lies below the theoretical value, the forest fire 
      hotspots are spatially dispersed at this distance."),
    p('(Please refer to "Overview" tab for interpretation of Statistical Test Result)')
  ))
}

# -----------------------
# Input Refresher Functions
# -----------------------
sc_refresh_city_inputs <- function(input, session) {
  return ({
    input$sc_province
    updateSelectInput(session, inputId="sc_city", label="City",
                      choices=c(unique(hotspot_data[[input$sc_province]]$city)),
                      selected=unique(hotspot_data[[input$sc_province]]$city)[1])
  })
}

sc_refresh_correction_inputs <- function(input, session) {
  return ({
    input$sc_function_type
    updateSelectInput(session, inputId="sc_correction_method",
                      label="Correction Method",
                      choices = c(switch(input$sc_function_type,
                                         "Fest" = F_correction_options,
                                         "Gest" = G_correction_options,
                                         "Kest" = K_L_correction_options,
                                         "Lest" = K_L_correction_options)),
                      selected="none")
  })
}

# -----------------------
# Main Server Function
# -----------------------
spatial_cluster_server <- function(input) {
  sc_city_csr <- eventReactive(input$sc_submit, {
    selected_month <- paste(input$sc_month, input$sc_year, sep="-")
    
    sc_point_data <<- hotspot_data[[input$sc_province]] %>%
      filter(city == input$sc_city) %>%
      filter(grepl(selected_month, date_str)) %>%
      drop_na()
    
    # print("Num of Rows: -------------")
    # print(nrow(sc_point_data))
    
    if(nrow(sc_point_data) < 10) {
      shinyalert("Too Little Data!", "Please select another location / month", type = "error")
      stop()
    }
    
    # Create Owin & Convert to ppp
    sc_city_boundary <- get_city_boundary(input$sc_province, input$sc_city)
    sc_city_ppp <- convert_to_spatstat(sc_city_boundary, sc_point_data)
    
    # Monte Carlo
    csr <- envelope(sc_city_ppp, switch(input$sc_function_type,
                                          "Fest" = Fest,
                                          "Gest" = Gest,
                                          "Kest" = Kest,
                                          "Lest" = Lest), nsim = input$sc_nsim)
    
    # print("CSR: -------------")
    # print(csr)
    
    return(csr)
  })
  
  return(
    renderPlot({
      csr <- sc_city_csr()
      plot(csr, main = "Graph")})
  )
}

# -----------------------
# Display Parameters Server Functions
# -----------------------
sc_param_location_server <- function(input) {
  return(
    renderText(paste0("Location: ", input$sc_city, ", ", input$sc_province))
  )
}

sc_param_date_server <- function(input) {
  return(
    renderText(paste0("Month: ", input$sc_month, "-", input$sc_year))
  )
}

sc_param_nrow_server <- function(input) {
  return(
    renderText(paste("Number of Data Points:", nrow(hotspot_data[[input$sc_province]] %>%
                                                      filter(city == input$sc_city) %>%
                                                      filter(grepl(paste(input$sc_month, input$sc_year, sep="-"), date_str)) %>%
                                                      drop_na())))
  )
}

sc_param_function_server <- function(input) {
  return(
    renderText(sprintf("Test Performed: %s", 
                       switch(input$sc_function_type,
                              "Fest" = "F Function",
                              "Gest" = "G Function",
                              "Kest" = "K Function",
                              "Lest" = "L Function")
    ))
  )
}

sc_param_sig_level_server <- function(input) {
  return(
    renderText(sprintf("Significance Level: %f", 2/(1+input$sc_nsim)))
  )
}