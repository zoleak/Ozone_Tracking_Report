##################################################################
### Define server for app ###
server <- function(input, output,session) {
  
  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
  session$onSessionEnded(function() {
    stopApp()
  })
  
  ##################################################################
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
  ###############################################################################  
  ### Create a vector of column names to be used in formatStyle() ###
  cols<-colnames(NY_area_daily_max[,7:250])
  cols3<-colnames(NY_design[,9:11])
  ### Call module from timeseriesplot script ###
  callModule(timeseriesplot,id="ny_options",data=wide_to_long_NY,data_wide=NY_area_daily_max,
             highest_level=NY_highest_levels,data_design=NY_design,
             data_design_wide = NY_design_plot,source_table="AirNow")
  ###############################################################################  
  ### Create a vector of column names to be used in formatStyle() ###
  cols<-colnames(PA_area_daily_max[,7:250])
  cols4<-colnames(PA_design[,9:11])
  ### Call module from timeseriesplot script ###
  callModule(timeseriesplot,id="pa_options",data = wide_to_long_PA,data_wide=PA_area_daily_max,
             highest_level=PA_highest_levels,data_design=PA_design,
             data_design_wide = PA_design_plot,source_table="AirNow")
  ##################################################################
  ### Call module from timeseriesplot script ###
  callModule(timeseriesplot, id = "nj_options",data=wide_to_long_NJ,data_wide=NJ_area_daily_max,
             highest_level =NJ_highest_levels,data_design=NJ_design,
             data_design_wide = NJ_design_plot,source_table="Envista")
  
} # Closes server function
##################################################################
### Run the application ###
#shinyApp(ui = ui, server = server)
