#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny)

source("home.R")
source("about.R")

# 
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Home", home_ui),
    tabPanel("About", about_ui)
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
