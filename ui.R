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
library(tibble)
library(rintrojs)
library(shinyalert)
library(shinyjs)
library(bsplus)
library(openxlsx)
#library(mailR)
##################################################################
#source("downloadButton.R")# Script that creates a module for download buttons throughout the app
source("timeseriesmod.R") # Script that creates tables,charts, etc for different tabPanels of app 
##################################################################
##################################################################  
### Create header for app ###
header<- dashboardHeaderPlus(title ="Ozone Tracking Report", titleWidth = 400,
                         dropdownMenu(type = "notifications",
                                      notificationItem(
                                        text = "Learn more about Design Values",
                                        href = "https://www.epa.gov/air-trends/air-quality-design-values",
                                        icon("exclamation-triangle"))),
                         left_menu = tagList(
                           dropdownBlock(
                             id = "mydropdown",
                             title = "Help",icon = "question-circle",
                             actionBttn(
                               "drop_help",
                               label = "See users guide",
                               icon = NULL,
                               style = "unite",
                               color = "default",
                               size = "sm"),
                             actionBttn(
                               "drop_tut",
                               label = "Start tutorial over",
                               icon = NULL,
                               style = "unite",
                               color = "default",
                               size = "sm")
                             )))
##################################################################
### Create sidebar for app ###
sidebar<-dashboardSidebar(
  sidebarMenu(#introBox(data.step = 2,data.intro = "This is a sidebar",
    menuItem("Home",tabName = "home",icon = icon("home")),
    menuItem("NJ Area Ozone",tabName = "nj",icon = icon("bar-chart-o")),
    menuItem("NY Area Ozone",tabName = "ny",icon = icon("bar-chart-o")),
    menuItem("PA Area Ozone",tabName = "pa",icon = icon("bar-chart-o")),
    menuItem("State Tally",tabName = "tally",icon = icon("bar-chart-o")),br(),
   tags$a(href="https://www.nj.gov/dep/airmon/", target="_blank",
           img(width= 100,height = 100,src="https://www.nj.gov/dep/awards/images/deplogoB.jpg",class="logo_pic"))
    
  )
)
##################################################################
##################################################################
### Create body for app ###
body<- dashboardBody(
  useShinyalert(),
  #Include the custom styling
  tags$head(
    #includeCSS("/Users/kevinzolea/Desktop/Ozone_Tracking_Report/www/custom.css")),
    #includeCSS("V:/air/monitoring/OZONE/Ozone Tracking Report/MyApplication/app/shiny/www/custom.css"),
    #includeCSS("V:/air/monitoring/Ozone Tracking Report/MyApplication/app/shiny/www/custom.css"),
    includeCSS("V:/air/monitoring/Ozone Tracking App/MyApplication/app/shiny/www/custom.css"),
    tags$script(HTML("
        // Enable navigation prompt
        window.onbeforeunload = function() {
            return 'Are you sure you want to exit?';
        };
    "))),
  introjsUI(),
  tabItems(
    tabItem("home",
            h1("Welcome to the Ozone Tracking Report App"),
            h2("Purpose of App:"),
            h3("This app can be used to display the current year statistics & design values for ozone for New Jersey and its surrounding 
               states. Users can view different summary tables and plots."),
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
    ),
    tabItem("tally",h3("Running Tally of the Number of Days the 70 ppb Ozone NAAQS was Exceeded in NJ's 
                       Non-Attainment Areas in 2022",style = "text-align: center;font-weight: bold;font-size: xx-large;
                       text-decoration: underline;"),br(),
            column(width=6,
                   use_bs_tooltip(),                     
                   bs_embed_tooltip(
            infoBoxOutput("tally_date"),title = "This is the current date that the data was updated")),br(),br(),br(),br(),
            column(width=4,offset = 4,box(width = 12,
            DT::dataTableOutput("all_states")%>%withSpinner(type = 1,color = "green"))))))
##################################################################
### Create ui ###
ui<-dashboardPage(skin = "green",
                  header = header,
                  sidebar = sidebar,
                  body = body
)  
##################################################################
