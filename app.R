### Script to create a shiny app for the Ozone Tracking Report ###
### Author: Kevin Zolea; 10/2019 ###
### Load in packages used to create everything in the app ###
library(shiny)
library(shinydashboard)
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
library(shinyBS)
##################################################################
### Read in Sites Data to plot sites on leaflet map ###
sites<-read_xlsx("Site_Locations.xlsx",col_names = T)%>%
      dplyr::distinct()
### Read in New Jersey County Shapefile ###
nj_counties<-st_read(getwd(),"New_Jersey_Counties")
### Change projection to work with leaflet map ###
nj_counties<-st_transform(nj_counties, crs="+init=epsg:4326")
### Read in PA County Shapefile ###
### Create vector of the counties we want so we can filter shapefile ###
tst<-c("BUCKS","CHESTER","PHILADELPHIA",
       "MONTGOMERY","DELAWARE")
pa_counties<-st_read(getwd(),"PaCounty2019_07")%>%
  dplyr::filter(COUNTY_NAM %in% tst)
### Change projection to work with leaflet map ###
pa_counties<-st_transform(pa_counties,crs="+init=epsg:4326")
### Read in CT County Shapefiles ###
### Create vector of the counties we want so we can filter shapefile ###
tst2<-c("Middlesex","Fairfield","New Haven")
ct_counties<-st_read(getwd(),"ct_county")%>%
  dplyr::filter(NAME10 %in% tst2)
### Change projection to work with leaflet map ###
ct_counties<-st_transform(ct_counties,crs="+init=epsg:4326")
### Read in Delaware county shapefile ###
del_counties<-st_read(getwd(),"delaware")
### Change projection to work with leaflet map ###
del_counties<-st_transform(del_counties,crs="+init=epsg:4326")
### Read in New York County Shapefile ###
### Create vector of the counties we wants so we can fitler shapefile ###
tst3<-c("New York","Suffolk","Bronx","Queens","Rockland","Richmond","Westchester")
ny_counties<-st_read(getwd(),"Counties")%>%
  dplyr::filter(NAME %in% tst3)
### Change projection to work with leaflet map ###
ny_counties<-st_transform(ny_counties,crs="+init=epsg:4326")
### Read in NJ ozone area daily max spreadsheet ###
NJ_area_daily_max<-read_xlsx("NJ Ozone 2019_KZ.xlsx",sheet = "DailyMax",skip = 3)%>%
  dplyr::slice(1:18)%>%
  setNames(., c("AQS Code","Latitude","State","Site Name","Elev (m)","County",
                                    format(as.Date(as.numeric(names(.)[-1:-6]), 
                                     origin = '1899-12-30'), '%m/%d/%Y')))
### Make all date columns numeric ###
NJ_area_daily_max[,7:252] <- sapply(NJ_area_daily_max[,7:252],as.numeric)

### Make another dataframe that makes wide to long ###
wide_to_long_NJ<-NJ_area_daily_max%>%
gather(Date,AQI_value,7:252)%>%
 dplyr::filter(!State == "NA")

### Read in NJ ozone area design values ###
NJ_design<-read_xlsx("NJ Ozone 2019_KZ.xlsx",sheet = "8Hr Rpt",skip = 3)%>%
  dplyr::select(1:11)%>%
  dplyr::slice(1:19)
### Read in NJ ozone highest levels ###
NJ_highest_levels<-read_xlsx("NJ Ozone 2019_KZ.xlsx",sheet = "NJ-8Hr",skip = 2)%>%
  dplyr::select(5:16)%>%
  dplyr::slice(1:19)
### Read in PA ozone daily max spreadsheet ###
PA_area_daily_max<-read_xlsx("PA-Area Ozone 2019 8hr_KZ.xlsx",sheet = "DailyMax",skip = 3)%>%
  dplyr::slice(1:24)%>%
  setNames(., c("AQS Code","Latitude","State","Site Name","Elev (m)","County",
                format(as.Date(as.numeric(names(.)[-1:-6]), 
                               origin = '1899-12-30'), '%m/%d/%Y')))
### Make all date columns numeric ###
PA_area_daily_max[,7:252] <- sapply(PA_area_daily_max[,7:252],as.numeric)

### Read in NY area daily max spreadsheet ###
NY_area_daily_max<-read_xlsx("NY-Area Ozone 2019 8hr_KZ.xlsx",sheet = "DailyMax",skip = 3)%>%
  dplyr::slice(1:27)%>%
  setNames(., c("AQS Code","Latitude","State","Site Name","Elev (m)","County",
                format(as.Date(as.numeric(names(.)[-1:-6]), 
                               origin = '1899-12-30'), '%m/%d/%Y')))
### Make all date columns numeric ###
NY_area_daily_max[,7:252] <- sapply(NY_area_daily_max[,7:252],as.numeric)

  
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
    menuItem("PA Area Ozone",tabName = "pa",icon = icon("bar-chart-o")),br(),
    HTML('<center> <img src="https://www.nj.gov/dep/awards/images/deplogoB.jpg"></center>')
    
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
            h2("Purpose of App:"),
            h3("This app can be used to understand the ozone trends for New Jersey and it's surrounding 
            states. Users can view different summary tables and track different ozone trends."),
            #actionButton("preview","Click to see map of sites")
            br(),
            h2("Map of Sites:"),br(),
            leafletOutput("site_map")%>%withSpinner(type = 1,color = "green")
            ),
    tabItem("nj",
            selectInput("nj_select","Please Select Summary Table:",
                        choices = c("Daily Max","8 Hour Design Values","Highest Levels")),
            awesomeCheckbox(
              inputId = "nj_wide",
              label = "Turn data from wide to long", 
              value = FALSE
            ),
            DT::dataTableOutput("dailytable")%>%withSpinner(type = 1,color = "green"),
            downloadButton('download_nj','Download Data')),
    
    tabItem("ny",
            selectInput("ny_select","Please Select Summary Table:",
                        choices = c("Daily Max","8 Hour Design Values","Highest Levels")),
            DT::dataTableOutput("dailytable2")%>%withSpinner(type = 1,color = "green"),
            downloadButton('download_ny','Download Data')),
   
    tabItem("pa",
            selectInput("pa_select","Please Select Summary Table:",
                        choices = c("Daily Max","8 Hour Design Values","Highest Levels")),
            DT::dataTableOutput("dailytable3")%>%withSpinner(type = 1,color = "green"),
            downloadButton('download_pa','Download Data'))))
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

### Allow users to download data for NJ daily max ###
  output$download_nj<-downloadHandler(
    filename = function(){
      paste("dataset-",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      write_csv(NJ_area_daily_max,file)
    })
###############################################################################    
### Allow users to download data for NJ daily max ###
  output$download_ny<-downloadHandler(
    filename = function(){
      paste("dataset-",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      write_csv(NY_area_daily_max,file)
    })
###############################################################################
### Allow users to download data for NJ daily max ###
  output$download_pa<-downloadHandler(
    filename = function(){
      paste("dataset-",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      write_csv(PA_area_daily_max,file)
    })
###############################################################################
### Create custom icon for markers on map ###
  aq_icon<-makeIcon(
    iconUrl = "https://www.pca.state.mn.us/sites/default/files/aqi-icon-airdata.png",
    iconWidth = 25 , iconHeight = 25,
    iconAnchorX =30 , iconAnchorY = 30)
###############################################################################  
### Create map of sites in ozone tracking report ###
  #observeEvent(input$preview,{
    #showModal(modalDialog(
     # title = "Map of sites",
  output$site_map<-renderLeaflet({
    leaflet(data = sites, options = leafletOptions(minZoom = 7))%>%
      addTiles()%>%
      addResetMapButton()%>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Grey")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
      #setView(lng = -74.4 ,lat =40, zoom = 8)%>%
      addMarkers(~LONGITUDE,~LATITUDE, popup = ~paste("<h4> Site ID:</h4>",SITE_ID,sep = ""),
                 icon = aq_icon)%>%
      addPolygons(data = nj_counties,popup= paste("County:",nj_counties$COUNTY,sep=""),color = "black",
                  fillColor = "#00FFFFFF",weight = 1,
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE))%>%
      addPolygons(data = pa_counties,popup= paste("County:",pa_counties$COUNTY_NAM,sep=""),color = "black",
                  fillColor = "#00FFFFFF",weight = 1,
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE))%>%
      addPolygons(data = ct_counties,popup= paste("County:",ct_counties$NAME10,sep=""),color = "black",
                  fillColor = "#00FFFFFF",weight = 1,
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE))%>%
      addPolygons(data = del_counties,popup= paste("County:",del_counties$NAME,sep=""),color = "black",
                  fillColor = "#00FFFFFF",weight = 1,
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE))%>%
      addPolygons(data = ny_counties,popup= paste("County:",ny_counties$NAME,sep=""),color = "black",
                  fillColor = "#00FFFFFF",weight = 1,
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE))%>%
      
      addLayersControl(
        baseGroups = c("OSM (default)", "Grey", "Satellite"),
        options = layersControlOptions(collapsed = FALSE))})#))})}
##################################################################
### Create a vector of column names to be used in formatStyle() ###
  cols<-colnames(NJ_area_daily_max[,7:250])
### Create summary table based on user input from drop down menu ###
output$dailytable<-renderDataTable({
  if(input$nj_select == "Daily Max" & input$nj_wide == FALSE){
  dat<-DT::datatable(NJ_area_daily_max,filter = 'top',
                  options = list(scrollX = TRUE,pageLength = 18),caption = "Colors: >70 ppb = Yellow,
                   >75 ppb = Orange, >84 ppb = Red",class = 'cell-border stripe')%>%
      formatStyle(cols,
      backgroundColor = styleEqual(c(71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87),
                                   c('#ffff00', '#ffff00','#ffff00','#ffff00',
                                     '#ffff00','#ff7e00','#ff7e00','#ff7e00',
                                     '#ff7e00','#ff7e00','#ff7e00','#ff7e00','#ff7e00',
                                     '#ff7e00','#ff0000','#ff0000','#ff0000')))
  
  return(dat)
    
  }
  
  
  else if(input$nj_select == "8 Hour Design Values"){
    DT::datatable(NJ_design,filter = 'top',
                  options = list(scrollX = TRUE,pageLength = 20),
                  class = 'cell-border stripe')
  }
  
  
  
  else if(input$nj_select == "Daily Max" & input$nj_wide == TRUE){
    
    DT::datatable(wide_to_long_NJ,filter = 'top',
                       options = list(scrollX = TRUE,pageLength = 18),
                      class = 'cell-border stripe')
    
    
  }
  
  else if (input$nj_select == "Highest Levels"){
    
    DT::datatable(NJ_highest_levels,filter = 'top',
                  options = list(scrollX = TRUE,pageLength = 19),
                  class = 'cell-border stripe')
  }


})
###############################################################################  
### Create a vector of column names to be used in formatStyle() ###
cols<-colnames(NY_area_daily_max[,7:250])
### Create summary table based on user input from drop down menu ###
output$dailytable2<-renderDataTable({
  if(input$ny_select == "Daily Max"){
    dat2<-DT::datatable(NY_area_daily_max,filter = 'top',
                       options = list(scrollX = TRUE,pageLength = 28),
                       class = 'cell-border stripe')%>%
      formatStyle(cols,
                  backgroundColor = styleEqual(c(71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87),
                                               c('#ffff00', '#ffff00','#ffff00','#ffff00',
                                                 '#ffff00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff7e00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff0000','#ff0000','#ff0000')))
    
    return(dat2)
    
  }
})
###############################################################################  
### Create a vector of column names to be used in formatStyle() ###
cols<-colnames(PA_area_daily_max[,7:250])
### Create summary table based on user input from drop down menu ###
output$dailytable3<-renderDataTable({
  if(input$pa_select == "Daily Max"){
    dat3<-DT::datatable(PA_area_daily_max,filter = 'top',
                       options = list(scrollX = TRUE,pageLength = 24),
                       class = 'cell-border stripe')%>%
      formatStyle(cols,
                  backgroundColor = styleEqual(c(71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87),
                                               c('#ffff00', '#ffff00','#ffff00','#ffff00',
                                                 '#ffff00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff7e00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff0000','#ff0000','#ff0000')))
    
    return(dat3)
    
  }
  
})
###############################################################################  
} # Closes server function
##################################################################
### Run the application ###
shinyApp(ui = ui, server = server)

