### Script to create a shiny app for the Ozone Tracking Report ###
### Author: Kevin Zolea; 9/2019 ###
### Load in packages used to create everything in the app ###
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plyr)
library(leaflet)
library(leaflet.extras)
library(readxl)
##################################################################
### Read in Sites Data to plot sites on leaflet map ###
sites<-read_xlsx("Site_Locations.xlsx",col_names = T)%>%
      dplyr::distinct()
### Create header for app ###
header<- dashboardHeader(title ="Ozone Tracking Report", titleWidth = 400,
                         dropdownMenu(type = "notifications",
                                      notificationItem(
                                        text = "Learn more about Design Values",
                                        href = "https://www.epa.gov/air-trends/air-quality-design-values",
                                        icon(""))))
##################################################################
### Create sidebar for app ###
sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Home",tabName = "home",icon = icon("home")),
    menuItem("NJ Area Ozone",tabName = "nj",icon = icon("bar-chart-o")),
    menuItem("NY Area Ozone",tabName = "ny",icon = icon("bar-chart-o")),
    menuItem("PA Area Ozone",tabName = "pa",icon = icon("bar-chart-o"))
  )
)
##################################################################
### Create body for app ###
body<- dashboardBody(
  
  # Include the custom styling
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem("home",
            h1("Welcome to the Ozone Tracking Report App!"),
            h3("Purpose of App:"),
            "This app can be used to understand the ozone trends for New Jersey and it's surrounding 
            states. Users can view different summary tables and track different ozone trends.",
            actionButton("preview","Click to see map of sites")),
    tabItem("nj",
            selectInput("nj_select","Please Select Summary Table:",
                        choices = c("8 Hour Design Values","Highest Levels",
                                    "Daily Max"))),
    
    tabItem("ny",
            selectInput("ny_select","Please Select Summary Table:",
                        choices = c("8 Hour Design Values","Highest Levels",
                                    "Daily Max"))),
   
    tabItem("pa",
            selectInput("pa_select","Please Select Summary Table:",
                        choices = c("8 Hour Design Values","Highest Levels",
                                    "Daily Max")))
  )
)
##################################################################
### Create ui ###
ui<-dashboardPage(skin = "green",
  header = header,
  sidebar = sidebar,
  body = body
  )  
##################################################################
### Define server for app ###
server <- function(input, output) {
### Create custom icon for markers on map ###
  aq_icon<-makeIcon(
    iconUrl = "https://www.pca.state.mn.us/sites/default/files/aqi-icon-airdata.png",
    iconWidth = 25 , iconHeight = 25,
    iconAnchorX =30 , iconAnchorY = 30)
###############################################################################  
### Create map of sites in ozone tracking report ###
  observeEvent(input$preview,{
    showModal(modalDialog(
      title = "Map of sites",
  output$map1<-renderLeaflet({
    leaflet(data = sites, options = leafletOptions(minZoom = 7))%>%
      addTiles()%>%
      addResetMapButton()%>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Grey")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
      #setView(lng = -74.4 ,lat =40, zoom = 8)%>%
      addMarkers(~LONGITUDE,~LATITUDE, popup = ~paste("<h4> Site ID:</h4>",SITE_ID,sep = ""),
                 icon = aq_icon)%>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Grey", "Satellite"),
        options = layersControlOptions(collapsed = FALSE))})))})}
##################################################################
### Run the application ###
shinyApp(ui = ui, server = server)

