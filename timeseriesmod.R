### This script creates a module that makes a ggplot graph with different widgets inside a shinydashboardPlus box ###
### Kevin Zolea ; 10/2019 ###

timeseriesplotUI<-function(id,data){
  ns <- NS(id)
  
  tagList(
    fluidRow(column(width = 6,
                    selectInput(ns("nj_select"),"Please Select Summary Table:",
                                choices = c("Daily 8-hr Max (ppb)","8 Hour Design Values (ppb)","7 Highest Daily 8-hr Max (ppb)"))),
                    #pickerInput(ns("nj_select"),"Please Select Summary Table:",
                    #            choices = c("Daily 8-hr Max","8 Hour Design Values (ppb)","7 Highest Daily 8-hr Max (ppb)"))),
             column(width = 6,
                    conditionalPanel("input.nj_wide == 1 && input.nj_select == 'Daily 8-hr Max (ppb)' ",ns=ns,
                                     jqui_draggable(
                                       boxPlus(
                                         title = "Plotting Options:",
                                         width = NULL,
                                         icon = "",
                                         collapsible = TRUE,
                                        conditionalPanel("input.nj_wide== 1",ns=ns,
                                                         selectizeInput(ns("site_name_select"),
                                                                                          "Choose Site:",choices = data$`Site Name`,
                                                                                          multiple = TRUE,options = list(
                                                                                            placeholder = "Please select a site below"
                                                                                          ))),
                                                          use_bs_tooltip(),                     
                                                          bs_embed_tooltip(
                                                          dateRangeInput(ns("dates"),"Choose Date Range:",
                                                          start = "2021-03-01",
                                                          end = "2021-10-31"),title = "Pick a date range to show on the plot/table below",
                                                          placement = "top"),
                                         use_bs_tooltip(),                     
                                         bs_embed_tooltip(
                                         plotOutput(ns("plot1")),
                                         title = "You can save the plot by right clicking and pressing save as")%>%withSpinner(type = 1,color = "green"),br(),
                                         chooseSliderSkin(skin = "Modern","green"),
                                         div(style = "width: 50%; margin: 0 auto;",
                                             use_bs_tooltip(),
                                             bs_embed_tooltip(
                                             sliderInput(ns("alpha"),"Select Shade of Lines:",min = 0,max = 0.65,value=0.5),
                                             title = "Use this slider to change the opacity of the lines",placement = "left")),
                                         #panel("Number of 
                                         #      days over exceedance:",verbatimTextOutput(ns("text1")))))),
                    panel(column(12,align = "center",dataTableOutput(ns("text1"),width = "75%"))))))
                    #conditionalPanel("input.nj_select == '8 Hour Design Values (ppb)'",ns=ns,
                    #                 selectizeInput(ns("site_design"),"Please Select Site:",choices = data$`Site Name`,
                    #                             multiple = TRUE,options = list(placeholder = "Please select a site below")))
                    #conditionalPanel("input.nj_select == '8 Hour Design Values (ppb)'",ns=ns,
                    #                 jqui_draggable(
                    #                   boxPlus(
                    #                   title = "Plotting Options:",
                    #                   width = NULL,
                    #                   icon = "",
                    #                   collapsible = TRUE,
                    #                   plotOutput(ns("plot2"))%>%withSpinner(type = 1,color = "green"))))
                    )),
    fluidRow(column(width = 8,
                    conditionalPanel("input.nj_select == 'Daily 8-hr Max (ppb)'",ns=ns,helpText("Click box below to get plotting options"),
                                     awesomeCheckbox(ns("nj_wide"),label = "Turn data from wide to long",
                                                     value = F)),
                    use_bs_tooltip(),                     
                    bs_embed_tooltip(
                    infoBoxOutput(ns("date_show")),title = "This is the current date that the data was updated"),
                    conditionalPanel("input.nj_select == '8 Hour Design Values (ppb)'",ns=ns,h1("Current year values are preliminary")))),
    fluidRow(
      img(src='test_legend.png',align="right",height="5%", width="20%"),
      DT::dataTableOutput(ns("dailytable"))%>%withSpinner(type = 1,color = "green"),
      use_bs_tooltip(),                     
      bs_embed_tooltip(
      downloadButton(ns("data_download"), label = "Download Data", class = "btn-primary"),
      title = "Click this button to download an excel spreadsheet of the data")))
}

timeseriesplot <- function(input, output, session, data, data_wide,data_design_excel ,highest_level, data_design,data_design_wide)
{
    ##################################################################
  ### Make dataframe reactive ###
  ### dataframe gets updated each time input from user is updated ###
  data1<-reactive({
    req(input$site_name_select)
    data%>%
      dplyr::filter(`Site Name` %in% input$site_name_select,
                    between(Date ,input$dates[1], input$dates[2]))
    
  })
  ##################################################################
  ### Make reactive dataframe for design values plot ###
  data2<-reactive({
    req(input$site_design)
    data_design_wide%>%
      dplyr::filter(Site %in% input$site_design)
  })
  ##################################################################
  #textshow<-reactive({
  #  req(input$site_name_select)
  #  data%>%
  #    dplyr::filter(`Site Name` %in% input$site_name_select,
  #                  `Concentration (PPB)` >70)%>%
  #    dplyr::group_by(`Site Name`)%>%
  #    dplyr::summarise(greater=n())
  #  
  #})
#
##################################################################
  ### Creates Time series plot for Daily 8-hr Max (ppb) data table ###
  output$plot1 <- renderPlot({
    req(input$site_name_select)
    req(input$dates)
    ggplot(data = data1(),aes(x=Date,y=`Concentration (PPB)`,
                                          color = data1()$`Site Name`))+
      geom_point(size = 3.5)+
      geom_hline(aes(yintercept = 70,linetype="70 ppb NAAQS"),
                 color="yellow",size = 2.2,alpha=input$alpha)+
      geom_hline(aes(yintercept = 75,linetype="75 ppb NAAQS"),
                 color="orange",size=2.2,alpha=input$alpha)+
      geom_hline(aes(yintercept = 84,linetype="84 ppb NAAQS"),
                 color="red",size=2.2,alpha=input$alpha)+
      ggtitle("Daily Maximum 8-Hr Ozone Concentration (ppb) in 2021")+
      labs(y= "Parts Per Billion (ppb)")+
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
      #scale_x_date(breaks = "1 month",date_labels = "%B")+
      scale_x_date(limits = c(input$dates[1], input$dates[2]))+
      scale_y_continuous(expand = c(0,0),limits = c(0, 95))+
      scale_linetype_manual(name = "", values = c(1, 1,1), 
                            guide = guide_legend(override.aes = list(color = c("yellow", "orange","red"))))
    
  })
  
#################################################################
  ### Create plot for Design Values tab ###
#  output$plot2 <- renderPlot({
#    req(input$site_design)
#    req(input$dates)
#
#    ggplot(data2(), aes(fill=data2()$Site, y=Design_Value, x=Year)) + 
#      geom_bar(position="dodge", stat="identity")+
#      geom_hline(aes(yintercept = 70,linetype="70 ppb NAAQS"),
#                              color="yellow",size = 2.2,alpha=input$alpha)+
#                   geom_hline(aes(yintercept = 75,linetype="75 ppb NAAQS"),
#                              color="orange",size=2.2,alpha=input$alpha)+
#                   ggtitle("8-Hour Air Quality Design-Values\n3 Year Average of 4th Highest Daily 8-Hour Maximum")+
#                   labs(y= "Parts Per Billion (ppb)",
#                        subtitle = "")+
#                   theme(plot.title=element_text(size=15, face="bold",vjust=0.5,hjust = 0.5),
#                         panel.grid.major.x = element_blank(),
#                         panel.grid.minor.x = element_blank(),
#                         legend.position = "bottom",
#                         legend.background = element_blank(),
#                         legend.text=element_text(size=10, face="bold"),
#                         legend.title = element_blank(),
#                         plot.subtitle = element_text(size=15, face="bold",vjust=0.5,hjust = 0.1),
#                         axis.title = element_text(face = "bold"),
#                         axis.text.x = element_text(face = "bold",size = 10),
#                         axis.text.y = element_text(face = "bold",size = 10))+
#                   scale_y_continuous(expand = c(0,0),limits = c(0, 90))+
#                    scale_linetype_manual(name = "", values = c(1, 1), 
#                            guide = guide_legend(override.aes = list(color = c("yellow", "orange"))))
#    
#    
#  })
##################################################################
  ### Create a vector of column names to be used in formatStyle() ###
  cols_wide<-colnames(NJ_area_daily_max_ex[,6:252])
  cols<-colnames(data_design[,9:11])
  cols_high<-colnames(highest_level[,3:12])
  ### Create summary table based on user input from drop down menu ###
  output$dailytable<-DT::renderDataTable({
    if(input$nj_select == "Daily 8-hr Max (ppb)" & input$nj_wide == FALSE){
      DT::datatable(data_wide,extensions = "FixedColumns",filter = "none",
                         options = list(scrollX = TRUE,scrollY = '500px',pageLength = 26,autoWidth = T,
                                        columnDefs = list(list(width = '100px',targets=c(5))),
                                        fixedColumns = list(leftColumns = 4)),
                         caption = htmltools::tags$caption(
                           style = 'caption-side: bottom; text-align: center;'
                           ,htmltools::em('Data Sources: NJ data from Envista, 
                                          Other states data from AirNow Tech')),
                         class = 'cell-border stripe')%>%
        formatStyle(cols_wide,
                    backgroundColor = styleEqual(c(71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,95),
                                                 c('#ffff00', '#ffff00','#ffff00','#ffff00',
                                                   '#ffff00','#ff7e00','#ff7e00','#ff7e00',
                                                   '#ff7e00','#ff7e00','#ff7e00','#ff7e00','#ff7e00',
                                                   '#ff7e00','#ff0000','#ff0000','#ff0000','#ff0000',
                                                   '#ff0000','#ff0000','#ff0000')),
                    textAlign = "center")%>%
        formatStyle(c(3),textAlign= "left")


    }
    else if(input$nj_select == "8 Hour Design Values (ppb)"){
      
      # a custom table container
      sketch <- withTags(
        table(
          class = "display",
          thead(
            tr(
              th(colspan = 3, "2021", style = "border-right: solid 2px;"),
              th(colspan = 5, "4th Max ppb", style = "border-right: solid 2px;"),
              th(colspan = 3, "Design Values")
            ),
            #tr(
             # th(colspan = 3, "", style = "border-right: solid 2px;")
              #th(colspan = 4, "AQS AMP450", style = "border-right: solid 2px;"),
              #th(source_table, style = "border-right: solid 2px;")
              #th(colspan = 3, "Preliminary")
            #),
            tr(
              th("AQS ID"),
              th("State"),
              th("Site", style = "border-right: solid 2px;"),
              th("2017"),
              th("2018"),
              th("2019"),
              th("2020", style = "border-right: solid 2px;"),
              th("2021", style = "border-right: solid 2px;"),
              th("2019"),
              th("2020"),
              th("2021")
            )
          )
        )
      )
      
      dat <- cbind(data_design[3:nrow(data_design),1:8], data_design[2:(nrow(data_design)-1), 9:11])
      
      h1("Current year values are preliminary")
      
      DT::datatable(dat,rownames = FALSE,filter = "none",
                    options = list(scrollX = TRUE,pageLength = 30),
                    class = 'cell-border stripe',
                    container = sketch,
                    caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: center;'
                      ,htmltools::em('Data Sources: NJ data from Envista, 
                                     Other states data from AirNow Tech')))%>%
        formatStyle(c(3,7,8), `border-right` = "solid 2px",textAlign = "center")%>%
        formatStyle(c(4,5,6),textAlign = "center")%>%
        formatStyle(c(3),textAlign = "left")%>%
        formatStyle(cols,
                    backgroundColor = styleEqual(c(71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87),
                                                c('#ffff00', '#ffff00','#ffff00','#ffff00',
                                                   '#ffff00','#ff7e00','#ff7e00','#ff7e00',
                                                   '#ff7e00','#ff7e00','#ff7e00','#ff7e00','#ff7e00',
                                                   '#ff7e00','#ff0000','#ff0000','#ff0000')),
                    textAlign = "center")
    }

    else if(input$nj_select == "Daily 8-hr Max (ppb)" & input$nj_wide == TRUE){
      
      DT::datatable(data,filter = "none",
                    options = list(scrollX = TRUE,pageLength = 18),
                    class = 'cell-border stripe',
                    caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: center;'
                      ,htmltools::em('Data Sources: NJ data from Envista, 
                                     Other states data from AirNow Tech')))
      
      
    }
    
    else if (input$nj_select == "7 Highest Daily 8-hr Max (ppb)"){
      

      DT::datatable(highest_level,filter = "none",
                    options = list(scrollX = TRUE,pageLength = 37),
                    class = 'cell-border stripe',
                    caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: center;'
                      ,htmltools::em('Data Sources: NJ data from Envista, 
                                     Other states data from AirNow Tech')))%>%
        formatStyle(cols_high,
                    backgroundColor = styleEqual(c(71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90),
                                                 c('#ffff00', '#ffff00','#ffff00','#ffff00',
                                                   '#ffff00','#ff7e00','#ff7e00','#ff7e00',
                                                   '#ff7e00','#ff7e00','#ff7e00','#ff7e00','#ff7e00',
                                                   '#ff7e00','#ff0000','#ff0000','#ff0000','#ff0000',
                                                   '#ff0000','#ff0000')),
                    textAlign = "center")%>%
        formatStyle('Site',target = 'row',backgroundColor=styleEqual(c('Max',18),c('grey','white')))
      
    }
    })
##################################################################
##################################################################
### Create info box to show what data is current to ###
  output$date_show<-renderInfoBox({
    infoBox(as.character(max(data$Date,na.rm = TRUE)),
            color = "green",icon = icon("calendar"),title = "Current to:",width = 6)
  })
##################################################################
##################################################################
  ### Allow users to download data ###
  output$data_download <- downloadHandler(
    filename = function() {
      if(input$nj_select == "8 Hour Design Values (ppb)"){
        "design_values_2021.xlsx"
        
      }
      else{
      paste("data-", Sys.Date(), ".csv", sep="")}
    },
    content = function(file) {
      if(input$nj_select == "Daily 8-hr Max (ppb)"){
      write.csv(data_wide, file)}
      else if (input$nj_select == "8 Hour Design Values (ppb)"){
        my_workbook <- createWorkbook()
        
        addWorksheet(
          wb = my_workbook,
          sheetName = "Sheet 1"
        )

        writeData(
          my_workbook,
          sheet = 1,
          data_design_excel,
          startRow = 1,
          startCol = 1
        )
        
     # conditionalFormatting(my_workbook, sheet = 1, cols = 9:11, rows = 4:31, rule = ">70",
      #                      style=createStyle(bgFill  = "#FFFF00"))
        
      #conditionalFormatting(my_workbook, sheet = 1, cols = 9:11, rows = 4:31, rule = ">75",
       #                     style=createStyle(bgFill  = "#ff7e00"))
      #conditionalFormatting(my_workbook, sheet = 1, cols = 4:11, rows = 4:30, rule = ">84",
      #                      style=createStyle(bgFill  = "#ff0000"))
      
        #Sets column widths
      setColWidths(my_workbook, sheet=1, cols = 1:ncol(data_design_excel), widths = "auto")
      
        # Saves workbook
        saveWorkbook(my_workbook, file)
    
       # write.csv(data_design,file)
      }
      else if (input$nj_select == "7 Highest Daily 8-hr Max (ppb)"){
        write.csv(highest_level,file)
        
      }
    }
  )
##################################################################
  ### Creates a pop window that only pops up when design value table is selected from dropdown, tells user that design values are preliminary ###
#  observe({
#    if(input$nj_select == "8 Hour Design Values (ppb)"){
#      showModal(modalDialog(
#        title = "",
#        div("Note: Design values are preliminary",style = "font-weight: bold;text-algin: center;
#            font-size: 150%;")
#        
#      ))}
#  })
##################################################################  
### Shows text output of number of days exceeding standard for each station based on user input ### 
  #output$text1<-renderText({
  #  if(input$site_name_select %in% textshow()$`Site Name`){
  #  print(paste(input$site_name_select,"=",textshow()$greater))}
  #  
  #  else if(textshow()$`Site Name` == "") {
  #    #print(paste(input$site_name_select,"= 0"))}
  #    print("")}
  #  
  #  
  #})
##################################################################  
  ### Create a datatable to show summary stats for each station under plot ###
  test<-reactive({
    data%>%
      dplyr::filter(`Site Name` %in% input$site_name_select,
                    between(Date ,input$dates[1], input$dates[2]))%>%
      dplyr::group_by(`Site Name`)%>%
      dplyr::summarise(Average = round(mean(`Concentration (PPB)`,na.rm = T)),
                       Median = round(median(`Concentration (PPB)`,na.rm = T)),
                       Max = max(`Concentration (PPB)`,na.rm = T),
                       Min = min(`Concentration (PPB)`,na.rm = T))
      
    
  })
  ##################################################################  
  ### Creates datatable under plot in Daily 8-hr Max (ppb) section ###
  output$text1<-renderDataTable({
   req(input$site_name_select)
    DT::datatable(test())%>%
    formatStyle(names(test()),fontWeight = "bold")
    
  })
  ##################################################################  
} # closes app









