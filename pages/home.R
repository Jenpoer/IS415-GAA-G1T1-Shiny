home_ui <- div(
  titlePanel("This is FINE: Fire INcidents Explorer"),
  h3("Overview"),
  p("FINE is an interactive web application tool that aids users in conducting 
    Spatial Point Pattern Analysis to analyse the spatial distribution of forest 
    fire hotspots in Sumatra, Indonesia between 2015 and 2019. Within this web application, 
    users can perform various types of Spatial Point Pattern Analyses such as 
    Exploratory Data Analysis, Kernel Density Estimation, Spatial Cluster Analysis, 
    and Spatiotemporal Area Analysis."),
  p("This application was created as part of Singapore Management University’s IS415 
    Geospatial Analytics & Applications 2023, under the guidance of Professor Kam Tin 
    Seong. The project aims to create an interactive web application that allows users 
    to carry out spatial point patterns analysis easily on forest fire hotspot data 
    from Indonesia without needing to write a single line of code."),
  h3("Background & Motivation"),
  p("Since the 1970s, Singapore has experienced bouts of haze due to slash-and-burn 
    forest clearing in Indonesia. These on-and-off occurrences compelled our group to 
    investigate the spread of forest fire events in Indonesia."),
  p("We therefore created FINE, a web app that uses geospatial analytics to visualise, 
    manipulate and analyze spatial forest fire hotspot data. This interactive and 
    consolidated platform makes it easier for not only ourselves, but others with 
    little to no GIS knowledge to access and analyse this forest fire data."),
  h3("Dataset Information"),
  p("This web application uses fire hotspot data from Sumatra, Indonesia between 2015 
    and 2019 (inclusive). This study period and area was chosen as it was during this 
    range of years that Singapore most recently suffered from significant haze, mostly 
    originating from forest fires in Sumatra."),
  p(HTML(paste0("The ", 
                a(href = 'https://staklim-riau.bmkg.go.id/infografis/detail/aTJhd0tCU1QxM2Z2dng5Q0phRTZFUT09', 
                  "Indonesian Meteorology, Climatology, and Geophysical Agency (2023)"),
                " defines hotspots as locations where the temperature detected by monitoring 
                satellites is higher than surrounding areas. As it is measured in a pre-defined 
                area, it does not directly correspond to the number of forest fire occurrences. 
                However, it still functions as a good indicator of forest fire incidents 
                and an estimation of the scale of the incident, and as such we found this 
                hotspot data to be suitable to be used in our Spatial Point Pattern Analysis tool."))),
  p("As such, the data for this webapp is equipped with is as follows:"),
  tags$ul(
    tags$li(HTML(paste0("Forest Fire Hotspot Data Set (2015-2019, Sumatra), from ", 
                        a(href = 'https://sipongi.menlhk.go.id/', 
                          "SiPongi+"), "."))), 
    tags$li(HTML(paste0("Sumatra Administrative Boundaries (2019), from ", 
                        a(href = 'https://www.indonesia-geospasial.com/2020/04/download-shapefile-shp-batas-desa.html', 
                          "Indonesia Geospasial"), ".")))
  ),
  h3("Features"),
  h4("1. Exploratory Data Analysis"),
  p("Users can use our Exploratory Data Analysis interface to interactively explore 
    the provided dataset. Our EDA tools include:"),
  tags$ol(
    tags$li("Point Map - An interactive point map for users to select and visualise 
            specific hotspot data from the available dataset"), 
    tags$li("Time Series - Shows the distribution of the frequency of  hotspot 
            occurrences over the course of a specific year")
  ),
  h4("2. Kernel Density Estimation"),
  p("Users are able to conduct first-order spatial point patterns analysis by 
    generating and viewing kernel density estimation maps of forest fire hotspots. 
    Kernel Density Estimation computes the intensity of point distribution using a 
    kernel function, and presents it in a visualisation known as a kernel denstiy map."),
  h4("3. Spatial Cluster Analysis"),
  p("Users are able to conduct second-order spatial point patterns analysis to identify 
    and statistically confirm possible interactions between events. Using the provided 
    test statistic functions, users can carry out the test for Complete Spatial Randomness, 
    which measures how points influence the location of one another."),
  p("The available functions are as follows:"),
  tags$ul(
    tags$li("F Function"), 
    tags$li("G Function"), 
    tags$li("K Function"), 
    tags$li("L Function")
  ),
  h4("4. Spatiotemporal Area Analysis"),
  p("Users are able to conduct various methods of spatiotemporal analysis by area in 
    order to better understand changes in hotspot point patterns over time. Using this 
    interface, users are able to view choropleth maps of aggregated fire hotspot data 
    at the sub-district level, as well as identify changes in spatial hot spots and 
    cold spots over time. Our spatiotemporial analysis tools include:"),
  tags$ol(
    tags$li("Mann-Kendall Test - Statistical test to identify increasing or 
            decreasing hotspot activity trends over time"), 
    tags$li("Emerging HotSpot Analysis - Use the Gi* statistic to categorise 
            subdisticts into hotspot/coldspot classifications"), 
    tags$li("Space-time K function - Used to examine and visualise the space-time 
            distribution of point events by subdistrict")
  ),
  h3("Useful Links"),
  p(a(href="https://is415-fire-incidents-explorer.netlify.app/user-guide.html", "User Guide")),
  p(HTML(paste0("For more information, please visit our  ", 
                a(href = 'https://is415-fire-incidents-explorer.netlify.app/', 
                  "project website"), ".")))
)