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
  # Opens user guide once user clicks button in help drop down 
  observeEvent(input$drop_help,{
                 showModal(modalDialog(
                   title = "Ozone Tracking Report App Users Guide",
                   h2("Introduction"),
                   "This app was built with the shiny package in R. Shiny is an open source R package that 
                   provides an elegant and powerful web framework for building web applications using R. Click
                   the image below to learn more.",br(),br(),
                   tags$a(href="https://shiny.rstudio.com/", target="_blank",
                   img(width='200px',height='150px',src="https://logodix.com/logo/689832.png")),
                   h2("Purpose of app"),
                   "The Ozone Tracking Report App is meant to display the current year statistics & design values 
                   for New Jersey and its surrounding states.",
                   h2("Understanding the app"),
                   "The core of the app can be found in the tabs of the sidebar. See below.", br(),br(),
                   img(src = 'app_tabs.png', height = '250px', width = '250px'),br(),br(),
                   "The home tab is where you can find an interactive map of all the sites of interest. You can zoom in or out
                   and click on the icons to find out the sites name. Click on the county shape and find out what county the site is in.
                   The NJ, NY, & PA area ozone tabs all do the same thing, however, they are just broken up into sections 
                   based on the areas of the sites. The State Tally tab is a running tally of the number of days the 70 ppb
                   ozone NAAQS was exceeded in NJ's non-attainmnet areas in 2020.",
                   h2("Contact Information"),
                   "This app was developed by the", a(href="https://www.nj.gov/dep/airmon/","Bureau of Air Monitoring.",
                                                      target="_blank"),"If you have any questions or suggestions please send them over to 
                   kevin.zolea@dep.nj.gov",
                   easyClose = TRUE))}
    
  )
##################################################################  
  # Pop up modal to welcome user to app and give them option to go through tutorial
  shinyalert("Welcome to the Ozone Tracking Report App","This app is meant to display the current year statistics & design values 
  for ozone for New Jersey and its surrounding states.

  You can start with a quick tour by clicking the button at the bottom or you can double click outside this box
             and try it for yourself.",
             type = "warning",
             confirmButtonCol = "#18C443",
             confirmButtonText = "Go to tutorial",
             closeOnClickOutside	
              = T)
  # Makes steps for tutorial
  steps <- reactive(
    data.frame(
      element=c("#site_map",".sidebar-menu",".logo_pic" ,".sidebar-toggle","#mydropdown",".fa-warning"),
      intro=c(
        "This is an interactive map that shows all the sites for NJ and its surrounding states. You can zoom in and out by clicking
        on the + or - symbol. If you click on the icon, you can find out the name of the site. If you click on the county shape, you can
        get the name of the county.",
        "This is the sidebar. You can click on the different tabs to go to different pages.",
        "Click on the DEP icon to bring you to the Bureau of Air Monitoring's home page.",
        "This is a button that allows you to close and open the sidebar",
        "Click this help button to either see the users guide or start this tutorial over again.",
        "Click on this button to show a drop down that you can click on to bring you to an
        EPA webpage explaining design values."
      ),
      position=c("bottom", "right","right","right", "bottom","bottom")
    )
  )
  # If user clicks button, it will lead them to tutorial
  observeEvent(input$shinyalert,
               introjs(session,
                       options = list(steps=steps(),
                                      "nextLabel"="Next",
                                      "prevLabel"="Previous",
                                      "skipLabel"="Skip"
                       )
               )
  )   
##################################################################  
 # If user clicks button, it will reopen the tutorial (help button drop down)
   observeEvent(input$drop_tut,{
    introjs(session,
            options = list(steps=steps(),
                           "nextLabel"="Next",
                           "prevLabel"="Previous",
                           "skipLabel"="Skip"
            )
    )
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
      addMarkers(~Longitude,~Latitude, popup = ~paste("<h4> Site ID:</h4>",`AQS-ID`,
                                                      "<h4> Site Name:</h4>",`Site Name`,sep = ""),
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
  callModule(timeseriesplot,id="ny_options",data=wide_to_long_NY,data_wide=NY_area_daily_max_ex,
             highest_level=NY_highest_levels,data_design=NY_design,data_design_excel=NY_design_ex,
             data_design_wide = NY_design_plot,source_table="AirNow")
  ###############################################################################  
  ### Create a vector of column names to be used in formatStyle() ###
  cols<-colnames(PA_area_daily_max[,7:250])
  cols4<-colnames(PA_design[,9:11])
  ### Call module from timeseriesplot script ###
  callModule(timeseriesplot,id="pa_options",data = wide_to_long_PA,data_wide=PA_area_daily_max_ex,
             highest_level=PA_highest_levels,data_design=PA_design,data_design_excel=PA_design_ex,
             data_design_wide = PA_design_plot,source_table="AirNow")
  ##################################################################
  ### Call module from timeseriesplot script ###
  callModule(timeseriesplot, id = "nj_options",data=wide_to_long_NJ,data_wide=NJ_area_daily_max_ex,
             highest_level =NJ_highest_levels,data_design=NJ_design,data_design_excel=NJ_design_ex,
             data_design_wide = NJ_design_plot,source_table="Envista")
##################################################################  
### Info box for State Tally Tab ###
    output$tally_date<-renderInfoBox({
        infoBox(as.character(max(wide_to_long_NJ$Date,na.rm = TRUE)),
                color = "green",icon = icon("calendar"),title = "Current to:",width = 6)
    })
##################################################################
### Make data frame for all states combined with how many days they are over exceedance ###
  
  all_states_df<-dplyr::bind_rows(wide_to_long_NJ,wide_to_long_NY,wide_to_long_PA)
  # Had to filter for each exceedance level and combine at end
    all_states_daily1<-all_states_df%>%
    dplyr::filter(`Concentration (PPB)` >70)%>%
    dplyr::group_by(State)%>%
    dplyr::summarise("Number of Exceedance Days" = n_distinct(Date),
                    "Days>70" = n_distinct(Date))

  all_states_daily2<-all_states_df%>%
    dplyr::filter(`Concentration (PPB)` >75)%>%
    dplyr::group_by(State)%>%
    dplyr::summarise("Days>75" = n_distinct(Date))

  all_states_daily3<-all_states_df%>%
    dplyr::filter(`Concentration (PPB)` >84)%>%
    dplyr::group_by(State)%>%
    dplyr::summarise("Days>84" = n_distinct(Date))

  all_states_daily<-left_join(all_states_daily1,all_states_daily2,by="State")
  
  all_states_daily<-left_join(all_states_daily,all_states_daily3,by="State")
  
  #add maryland, since it has zero exceedances and doesn't show up
  all_states_daily<-all_states_daily%>%
    add_row(State="MD")
  
  output$all_states<-DT::renderDataTable({
    DT::datatable(all_states_daily,rownames = FALSE,options = list(dom = 't',
       columnDefs = list(
        list(className = "nowrap", targets = "_all",class = 'cell-border stripe'))))%>%
      formatStyle(names(all_states_daily),textAlign = "center",
                  `border` = "solid 1px")
  })
##################################################################
 # Send email based on exceedance from NJ data
#  observe({
#    if(wide_to_long_NJ$`Concentration (PPB)`> 70){
      
      
#    }
#  })
##################################################################
  
  
} # Closes server function
##################################################################
### Run the application ###
#shinyApp(ui = ui, server = server)
