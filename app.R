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
##################################################################
source("downloadButton.R")# Script that creates a module for download buttons throughout the app 
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
 dplyr::filter(!State == "NA",!Date == "NA")%>%
 dplyr::mutate(Date = as.Date(Date,format = "%m/%d/%Y"))

### Read in NJ ozone area design values ###
NJ_design<-read_xlsx("NJ Ozone 2019_KZ.xlsx",sheet = "8Hr Rpt",skip = 3)%>%
  dplyr::select(1:11)%>%
  dplyr::slice(1:19)
### Read in NJ ozone highest levels ###
NJ_highest_levels<-read_xlsx("NJ Ozone 2019_KZ.xlsx",sheet = "NJ-8Hr",skip = 2)%>%
  dplyr::select(5:16)#%>%
  #dplyr::slice(1:19)
### Read in NY area daily max spreadsheet ###
NY_area_daily_max<-read_xlsx("NY-Area Ozone 2019 8hr_KZ.xlsx",sheet = "DailyMax",skip = 3)%>%
  dplyr::slice(1:27)%>%
  setNames(., c("AQS Code","Latitude","State","Site Name","Elev (m)","County",
                format(as.Date(as.numeric(names(.)[-1:-6]), 
                               origin = '1899-12-30'), '%m/%d/%Y')))
### Make all date columns numeric ###
NY_area_daily_max[,7:252] <- sapply(NY_area_daily_max[,7:252],as.numeric)
### Read in NY area design values spreadsheet ###
NY_design<-read_xlsx("NY-Area Ozone 2019 8hr_KZ.xlsx",sheet = "8Hr Rpt",skip = 3)%>%
  dplyr::select(1:11)%>%
  dplyr::slice(1:30)
### Read in NY area highest levels speadsheet ###
NY_highest_levels<-read_xlsx("NY-Area Ozone 2019 8hr_KZ.xlsx",sheet = "NY-8Hr",skip = 2)%>%
  dplyr::select(5:16)

### Read in PA ozone daily max spreadsheet ###
PA_area_daily_max<-read_xlsx("PA-Area Ozone 2019 8hr_KZ.xlsx",sheet = "DailyMax",skip = 3)%>%
  dplyr::slice(1:24)%>%
  setNames(., c("AQS Code","Latitude","State","Site Name","Elev (m)","County",
                format(as.Date(as.numeric(names(.)[-1:-6]), 
                               origin = '1899-12-30'), '%m/%d/%Y')))
### Make all date columns numeric ###
PA_area_daily_max[,7:252] <- sapply(PA_area_daily_max[,7:252],as.numeric)
### Read in PA design values spreadsheet ###
PA_design<-read_xlsx("PA-Area Ozone 2019 8hr_KZ.xlsx",sheet = "8Hr Rpt",skip = 3)%>%
  dplyr::select(1:11)%>%
  dplyr::slice(1:25)
### Read in PA Highest Level spreadsheet ###
PA_highest_levels<-read_xlsx("PA-Area Ozone 2019 8hr_KZ.xlsx",sheet = "PA-8Hr",skip = 2)%>%
  dplyr::select(5:16)
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
    #includeCSS("/Users/kevinzolea/Desktop/Temp_Impairments/www/styles.css")),
    includeCSS("C:/Users/kzolea/Desktop/Ozone_Tracking_Report/www/custom.css")),
  
  tabItems(
    tabItem("home",
            h1("Welcome to the Ozone Tracking Report App!"),
            h2("Purpose of App:"),
            h3("This app can be used to understand the ozone trends for New Jersey and it's surrounding 
            states. Users can view different summary tables and track different ozone trends."),
            br(),
            h2("Map of Sites:"),br(),
            leafletOutput("site_map")%>%withSpinner(type = 1,color = "green")
            ),
    tabItem("nj",
            fluidRow(column(width = 6,
            selectInput("nj_select","Please Select Summary Table:",
                        choices = c("Daily Max","8 Hour Design Values","Highest Levels"))),
            column(width = 6,
            conditionalPanel("input.nj_wide ==1 && input.nj_select == 'Daily Max' ",
            jqui_draggable(
            boxPlus(
              title = "Plotting Options:",
              width = NULL,
              icon = "",
              collapsible = TRUE,
              conditionalPanel("input.nj_wide== 1",selectInput("site_name_select",
                                                               "Choose Site:",choices = wide_to_long_NJ$`Site Name`,
                                                               multiple = TRUE,selected = "Ancora State Hospital"
              ),dateRangeInput("dates","Choose Date Range:",
                               start = "2019-03-01",
                               end = "2019-10-07"),
              plotOutput("plot1")%>%withSpinner(type = 1,color = "green"),br(),
              setSliderColor("green",c(1)),
              div(style = "width: 50%; margin: 0 auto;",
              sliderInput("alpha","Select Shade of Lines:",min = 0,max = 0.65,value=0.5)))))))),
            fluidRow(column(width = 6,
            conditionalPanel("input.nj_select == 'Daily Max'",helpText("Click box below to get plotting options"),
                awesomeCheckbox(inputId = "nj_wide",label = "Turn data from wide to long",
                                value = F)),
            infoBoxOutput("date_show"))),
          #conditionalPanel("input.nj_wide== 1",selectInput("site_name_select",
           #                                                "Choose site:",choices = wide_to_long_NJ$`Site Name`)),
            fluidRow(
            DT::dataTableOutput("dailytable")%>%withSpinner(type = 1,color = "green"),
            downloadObjUI(id="nj_download1"))),
    
    tabItem("ny",
            selectInput("ny_select","Please Select Summary Table:",
                        choices = c("Daily Max","8 Hour Design Values","Highest Levels")),
            DT::dataTableOutput("dailytable2")%>%withSpinner(type = 1,color = "green"),
            downloadObjUI(id="ny_download1")),
   
    tabItem("pa",
            selectInput("pa_select","Please Select Summary Table:",
                        choices = c("Daily Max","8 Hour Design Values","Highest Levels")),
            DT::dataTableOutput("dailytable3")%>%withSpinner(type = 1,color = "green"),
            downloadObjUI(id="pa_download1"))))
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
##################################################################
### Make dataframe reactive ###
### dataframe gets updated each time input from user is updated ###
  wide_long_nj_reac<-reactive({
    req(input$site_name_select)
    wide_to_long_NJ%>%
      dplyr::filter(`Site Name` ==input$site_name_select,
                    between(Date ,input$dates[1], input$dates[2]))

  })
##################################################################
### Allow users to download data for NJ daily max ###
 callModule(downloadObj,id = "nj_download1",data=NJ_area_daily_max)
###############################################################################    
### Allow users to download data for PA daily max ###
  callModule(downloadObj,id = "pa_download1",data=PA_area_daily_max)
###############################################################################
### Allow users to download data for PA daily max ###
  callModule(downloadObj,id = "ny_download1",data=NY_area_daily_max)
###############################################################################
### Create custom icon for markers on map ###
  aq_icon<-makeIcon(
    iconUrl = "https://www.pca.state.mn.us/sites/default/files/aqi-icon-airdata.png",
    iconWidth = 25 , iconHeight = 25,
    iconAnchorX =30 , iconAnchorY = 30)
###############################################################################  
### Create map of sites in ozone tracking report ###
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
  cols2<-colnames(NJ_design[,9:11])
### Create summary table based on user input from drop down menu ###
##################################################################
output$dailytable<-renderDataTable({
  if(input$nj_select == "Daily Max" & input$nj_wide == FALSE){
  dat<-DT::datatable(NJ_area_daily_max,filter = 'top',
                  options = list(scrollX = TRUE,pageLength = 18),
                  caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: center;'
                    ,htmltools::em('Data Sources: NJ data from Envista, 
                                              Other states data from AirNow Tech')),
                  class = 'cell-border stripe')%>%
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
                  class = 'cell-border stripe',
                  caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: center;'
                    ,htmltools::em('Data Sources: NJ data from Envista, 
                                              Other states data from AirNow Tech')))%>%
      formatStyle(cols2,
                  backgroundColor = styleEqual(c(71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87),
                                               c('#ffff00', '#ffff00','#ffff00','#ffff00',
                                                 '#ffff00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff7e00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff0000','#ff0000','#ff0000')))
  }
  
  
  
  else if(input$nj_select == "Daily Max" & input$nj_wide == TRUE){
    
    DT::datatable(wide_to_long_NJ,filter = 'top',
                       options = list(scrollX = TRUE,pageLength = 18),
                      class = 'cell-border stripe',
                  caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: center;'
                    ,htmltools::em('Data Sources: NJ data from Envista, 
                                   Other states data from AirNow Tech')))
    
    
  }
  
  else if (input$nj_select == "Highest Levels"){
    
    DT::datatable(NJ_highest_levels,filter = 'top',
                  options = list(scrollX = TRUE,pageLength = 24),
                  class = 'cell-border stripe',
                  caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: center;'
                    ,htmltools::em('Data Sources: NJ data from Envista, 
                                   Other states data from AirNow Tech')))
  }


})
##################################################################
###############################################################################  
### Create a vector of column names to be used in formatStyle() ###
cols<-colnames(NY_area_daily_max[,7:250])
cols3<-colnames(NY_design[,9:11])
### Create summary table based on user input from drop down menu ###
output$dailytable2<-renderDataTable({
  if(input$ny_select == "Daily Max"){
    dat2<-DT::datatable(NY_area_daily_max,filter = 'top',
                       options = list(scrollX = TRUE,pageLength = 28),
                       class = 'cell-border stripe',
                       caption = htmltools::tags$caption(
                         style = 'caption-side: bottom; text-align: center;'
                         ,htmltools::em('Data Sources: NJ data from Envista, 
                                        Other states data from AirNow Tech')))%>%
      formatStyle(cols,
                  backgroundColor = styleEqual(c(71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87),
                                               c('#ffff00', '#ffff00','#ffff00','#ffff00',
                                                 '#ffff00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff7e00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff0000','#ff0000','#ff0000')))
    
    return(dat2)
    
  }
  else if(input$ny_select == "8 Hour Design Values"){
    DT::datatable(NY_design,filter = 'top',
                  options = list(scrollX = TRUE,pageLength = 30),
                  class = 'cell-border stripe',
                  caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: center;'
                    ,htmltools::em('Data Sources: NJ data from Envista, 
                                              Other states data from AirNow Tech')))%>%
      formatStyle(cols3,
                  backgroundColor = styleEqual(c(71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87),
                                               c('#ffff00', '#ffff00','#ffff00','#ffff00',
                                                 '#ffff00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff7e00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff0000','#ff0000','#ff0000')))
  }
  else if(input$ny_select == "Highest Levels"){
    DT::datatable(NY_highest_levels,filter = 'top',
                  options = list(scrollX = TRUE,pageLength = 37),
                  class = 'cell-border stripe',
                  caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: center;'
                    ,htmltools::em('Data Sources: NJ data from Envista, 
                                   Other states data from AirNow Tech')))
  }
})
###############################################################################  
### Create a vector of column names to be used in formatStyle() ###
cols<-colnames(PA_area_daily_max[,7:250])
cols4<-colnames(PA_design[,9:11])
### Create summary table based on user input from drop down menu ###
output$dailytable3<-renderDataTable({
  if(input$pa_select == "Daily Max"){
    dat3<-DT::datatable(PA_area_daily_max,filter = 'top',
                       options = list(scrollX = TRUE,pageLength = 24),
                       class = 'cell-border stripe',
                       caption = htmltools::tags$caption(
                         style = 'caption-side: bottom; text-align: center;'
                         ,htmltools::em('Data Sources: NJ data from Envista, 
                                              Other states data from AirNow Tech')))%>%
      formatStyle(cols,
                  backgroundColor = styleEqual(c(71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87),
                                               c('#ffff00', '#ffff00','#ffff00','#ffff00',
                                                 '#ffff00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff7e00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff0000','#ff0000','#ff0000')))
    
    return(dat3)
    
  }
  
  else if(input$pa_select == "8 Hour Design Values"){
    DT::datatable(PA_design,filter = 'top',
                  options = list(scrollX = TRUE,pageLength = 30),
                  class = 'cell-border stripe',
                  caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: center;'
                    ,htmltools::em('Data Sources: NJ data from Envista, 
                                              Other states data from AirNow Tech')))%>%
      formatStyle(cols3,
                  backgroundColor = styleEqual(c(71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87),
                                               c('#ffff00', '#ffff00','#ffff00','#ffff00',
                                                 '#ffff00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff7e00','#ff7e00','#ff7e00','#ff7e00',
                                                 '#ff7e00','#ff0000','#ff0000','#ff0000')))
  }
  
  else if (input$pa_select == "Highest Levels"){
    DT::datatable(PA_highest_levels,filter = 'top',
                  options = list(scrollX = TRUE,pageLength = 37),
                  class = 'cell-border stripe',
                  caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: center;'
                    ,htmltools::em('Data Sources: NJ data from Envista, 
                                   Other states data from AirNow Tech')))
  }
  
})
##################################################################
### Creates a pop window that only pops up when design value table is selected from dropdown, tells user that design values are preliminary ###
observe({
  if(input$nj_select == "8 Hour Design Values" || input$pa_select == "8 Hour Design Values"
     || input$ny_select == "8 Hour Design Values"){
  showModal(modalDialog(
    title = "",
    div("Note: Design values are preliminary",style = "font-weight: bold;text-algin: center;
        font-size: 150%;")
  
  ))}
})
##################################################################
### Create info box to show what data is current to ###
output$date_show<-renderInfoBox({
  req(input$dates)
  infoBox(as.character(max(wide_long_nj_reac()$Date,na.rm = TRUE)),
          color = "green",icon = icon("calendar"),title = "Current to:",width = 6)
})
##################################################################
### Creates Time series plot for Daily Max data table ###
output$plot1<-renderPlot({
  req(input$site_name_select)
  req(input$dates)
  ggplot(data = wide_long_nj_reac(),aes(x=Date,y=AQI_value,
                                        color = wide_long_nj_reac()$`Site Name`))+
    geom_point(size = 3.5)+
    geom_hline(aes(yintercept = 70,linetype="70 ppb NAAQS"),
               color="yellow",size = 2.2,alpha=input$alpha)+
    geom_hline(aes(yintercept = 75,linetype="75 ppb NAAQS"),
               color="orange",size=2.2,alpha=input$alpha)+
    ggtitle("Daily Ozone AQI Values in 2019")+
    labs(y= "AQI Value")+
      theme(plot.title=element_text(size=15, face="bold",vjust=0.5,hjust = 0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = "bottom",
            legend.background = element_blank(),
            legend.text=element_text(size=10, face="bold"),
            legend.title = element_blank(),
            plot.subtitle = element_text(size=15, face="bold",vjust=0.5,hjust = 0.1),
            axis.title = element_text(face = "bold"),
            axis.text.x = element_text(face = "bold",size = 10),
            axis.text.y = element_text(face = "bold",size = 10))+
    scale_x_date(breaks = "1 month",date_labels = "%B")+
    scale_y_continuous(expand = c(0,0),limits = c(0, 90))+
    scale_linetype_manual(name = "", values = c(1, 1), 
                          guide = guide_legend(override.aes = list(color = c("yellow", "orange"))))
})
} # Closes server function
##################################################################
### Run the application ###
shinyApp(ui = ui, server = server)

