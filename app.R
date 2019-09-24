### Script to create a shiny app for the Ozone Tracking Report ###
### Author: Kevin Zolea; 9/2019 ###
### Load in packages used to create everything in the app ###
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plyr)
##################################################################
### Create header for app ###
header<- dashboardHeader(title = "Ozone Tracking Report",titleWidth = 300,
                         dropdownMenu(type = "notifications",
                                      notificationItem(
                                        text = "Learn more about Design Values",
                                        href = "https://www.epa.gov/air-trends/air-quality-design-values",
                                        icon(""))))
### Create sidebar for app ###
sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Home",tabName = "home",icon = icon("home")),
    menuItem("NJ Ozone",tabName = "nj",icon = icon("bar-chart-o")),
    menuItem("NY Area Ozone",tabName = "ny",icon = icon("bar-chart-o")),
    menuItem("PA Area Ozone",tabName = "pa",icon = icon("bar-chart-o"))
  )
)
### Create body for app ###
body<- dashboardBody(
  tabItems(
    tabItem("home",
            h1("Welcome to the Ozone Tracking Report App!"),
            h3("Purpose of App:")),
    tabItem("nj"),
    tabItem("ny"),
    tabItem("pa")
  )
)
### Create ui ###
ui<- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
  
)  

##################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
   

}
##################################################################
# Run the application 
shinyApp(ui = ui, server = server)

