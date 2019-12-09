### Script to create a shiny app for the Ozone Tracking Report ###
### Author: Kevin Zolea; 10/2019 ###
### Load in packages used to create everything in the app ###
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(dplyr)
library(plyr)
library(leaflet)
library(leaflet.extras)
library(readxl)
library(shinycssloaders)
library(sf)
library(DT)
library(readr)
library(tidyr)
library(shinyWidgets)
library(shinyjqui)
library(htmltools)
##################################################################
#source("downloadButton.R")# Script that creates a module for download buttons throughout the app
source("timeseriesmod.R") # Script that creates tables,charts, etc for different tabPanels of app 
##################################################################
##################################################################  
### Create header for app ###
header<- dashboardHeader(title ="Ozone Tracking Report", titleWidth = 400,
                         dropdownMenu(type = "notifications",
                                      notificationItem(
                                        text = "Learn more about Design Values",
                                        href = "https://www.epa.gov/air-trends/air-quality-design-values",
                                        icon("exclamation-triangle"))))
##################################################################
### Create sidebar for app ###
sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Home",tabName = "home",icon = icon("home")),
    menuItem("NJ Area Ozone",tabName = "nj",icon = icon("bar-chart-o")),
    menuItem("NY Area Ozone",tabName = "ny",icon = icon("bar-chart-o")),
    menuItem("PA Area Ozone",tabName = "pa",icon = icon("bar-chart-o")),br(),
    tags$a(href="http://www.njaqinow.net/", target="_blank",
           img(width= 100,height = 100,src="https://www.nj.gov/dep/awards/images/deplogoB.jpg",class="logo_pic"))
    
  )
)
##################################################################
##################################################################
### Create body for app ###
body<- dashboardBody(
  
  #Include the custom styling
  tags$head(
    #includeCSS("/Users/kevinzolea/Desktop/Ozone_Tracking_Report/www/custom.css")),
    includeCSS("V:/air/monitoring/OZONE/Ozone Tracking Report/MyApplication/app/shiny/www/custom.css")),
  
  tabItems(
    tabItem("home",
            h1("Welcome to the Ozone Tracking Report App"),
            h2("Purpose of App:"),
            h3("This app can be used to understand the ozone trends for New Jersey and it's surrounding 
               states. Users can view different summary tables and track different ozone trends."),
            br(),
            h2("Map of Sites:"),br(),
            leafletOutput("site_map")%>%withSpinner(type = 1,color = "green")
    ),
    tabItem("nj",
            timeseriesplotUI("nj_options",data = wide_to_long_NJ)),
    
    tabItem("ny",
            timeseriesplotUI("ny_options",data = wide_to_long_NY)
    ),
    
    tabItem("pa",
            timeseriesplotUI("pa_options",data = wide_to_long_PA)
    )))
##################################################################
### Create ui ###
ui<-dashboardPage(skin = "green",
                  header = header,
                  sidebar = sidebar,
                  body = body
)  
##################################################################