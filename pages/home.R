home_ui <- div(
  titlePanel("This is FINE: Fire Incidents Explorer"),
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
  h3("Features"),
  h4("1. Exploratory Data Analysis"),
  p("Users can use this our Exploratory Data Analysis interface to interactively explore 
    the provided dataset. Our EDA tools include:"),
  tags$ol(
    tags$li("Point Map - An interactive point map for users to select and visualise 
            specific hotspot data from the available dataset"), 
    tags$li("Time Series - Shows the distribution of the frequency of  hotspot 
            occurrences over the course of a specific year")
  ),
  h4("2. Kernel Density Estimation"),
  p("Users are able to conduct first-order spatial point patterns analysis by viewing 
    kernel density estimation maps of forest fire hotspots."),
  h4("3. Spatial Cluster Analysis"),
  p("Users are able to conduct second-order spatial point patterns analysis to identify 
    and statistically confirm possible interactions between events."),
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
  p(HTML(paste0("For more information, please visit our  ", 
                a(href = 'https://is415-fire-incidents-explorer.netlify.app/', 
                  "project website"), ".")))
)