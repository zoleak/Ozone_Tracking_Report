### This script creates a module that makes a ggplot graph with different widgets inside a shinydashboardPlus box ###
### Kevin Zolea ; 10/2019 ###

timeseriesplotUI<-function(id,data){
  ns <- NS(id)
  
  tagList(
    fluidRow(column(width = 6,
                    selectInput(ns("nj_select"),"Please Select Summary Table:",
                                choices = c("Daily Max","8 Hour Design Values","Highest Levels"))),
             column(width = 6,
                    conditionalPanel("input.nj_wide == 1 && input.nj_select == 'Daily Max' ",ns=ns,
                                     jqui_draggable(
                                       boxPlus(
                                         title = "Plotting Options:",
                                         width = NULL,
                                         icon = "",
                                         collapsible = TRUE,
                                         conditionalPanel("input.nj_wide== 1",ns=ns,
                                                          selectInput(ns("site_name_select"),
                                                                                          "Choose Site:",choices = data$`Site Name`,
                                                                                          multiple = TRUE,selected = "Ancora State Hospital")),
                                                          dateRangeInput(ns("dates"),"Choose Date Range:",
                                                          start = "2019-03-01",
                                                          end = "2019-10-07"),
                                         plotOutput(ns("plot1"))%>%withSpinner(type = 1,color = "green"),br(),
                                         chooseSliderSkin(skin = "Modern","green"),
                                         div(style = "width: 50%; margin: 0 auto;",
                                             sliderInput(ns("alpha"),"Select Shade of Lines:",min = 0,max = 0.65,value=0.5))))),
                    conditionalPanel("input.nj_select == '8 Hour Design Values'",ns=ns,
                                     selectInput(ns("site_design"),"Please Select Site:",choices = data$`Site Name`,
                                                 multiple = TRUE)),
                    conditionalPanel("input.nj_select == '8 Hour Design Values'",ns=ns,
                                     boxPlus(
                                       title = "Plotting Options:",
                                       width = NULL,
                                       icon = "",
                                       collapsible = TRUE,
                                     plotOutput(ns("plot2"))%>%withSpinner(type = 1,color = "green"))))),
    fluidRow(column(width = 6,
                    conditionalPanel("input.nj_select == 'Daily Max'",ns=ns,helpText("Click box below to get plotting options"),
                                     awesomeCheckbox(ns("nj_wide"),label = "Turn data from wide to long",
                                                     value = F)),
                    infoBoxOutput(ns("date_show")))),
    fluidRow(
      DT::dataTableOutput(ns("dailytable"))%>%withSpinner(type = 1,color = "green"),
      downloadButton(ns("data_download"), label = "Download Data", class = "btn-primary"))
  )
}

timeseriesplot <- function(input, output, session, data, data_wide, highest_level, data_design)
{
  ##################################################################
  ### Make dataframe reactive ###
  ### dataframe gets updated each time input from user is updated ###
  data1<-reactive({
    req(input$site_name_select)
    data%>%
      dplyr::filter(`Site Name` ==input$site_name_select,
                    between(Date ,input$dates[1], input$dates[2]))
    
  })
  ##################################################################
  ### Make reactive dataframe for design values plot ###
  data2<-reactive({
    req(input$site_design)
    data_design%>%
      dplyr::filter(Site == input$site_design)
  })
  
  ##################################################################
  ### Creates Time series plot for Daily Max data table ###
  output$plot1 <- renderPlot({
    req(input$site_name_select)
    req(input$dates)
    ggplot(data = data1(),aes(x=Date,y=AQI_value,
                                          color = data1()$`Site Name`))+
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
  
  ##################################################################
  ### Create plot for Design Values tab ###
  output$plot2 <- renderPlot({
    req(input$site_name_select)
    req(input$dates)
    ggplot(data = data2(),aes(x=Date,y=AQI_value,
                              color = data2()$`Site Name`))+
      geom_line(size = 3.5)+
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
  ##################################################################
  ### Create a vector of column names to be used in formatStyle() ###
  #cols<-colnames(NJ_area_daily_max[,7:250])
  cols<-colnames(data_design[,9:11])
  ### Create summary table based on user input from drop down menu ###
  output$dailytable<-renderDataTable({
    if(input$nj_select == "Daily Max" & input$nj_wide == FALSE){
      DT::datatable(data_wide,filter = 'top',
                         options = list(scrollX = TRUE,pageLength = 18),
                         caption = htmltools::tags$caption(
                           style = 'caption-side: bottom; text-align: center;'
                           ,htmltools::em('Data Sources: NJ data from Envista, 
                                          Other states data from AirNow Tech')),
                         class = 'cell-border stripe')
      

    }
    else if(input$nj_select == "8 Hour Design Values"){
      DT::datatable(data_design,filter = 'top',
                    options = list(scrollX = TRUE,pageLength = 20),
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
    }
    else if(input$nj_select == "Daily Max" & input$nj_wide == TRUE){
      
      DT::datatable(data,filter = 'top',
                    options = list(scrollX = TRUE,pageLength = 18),
                    class = 'cell-border stripe',
                    caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: center;'
                      ,htmltools::em('Data Sources: NJ data from Envista, 
                                     Other states data from AirNow Tech')))
      
      
    }
    
    else if (input$nj_select == "Highest Levels"){
      
      DT::datatable(highest_level,filter = 'top',
                    options = list(scrollX = TRUE,pageLength = 24),
                    class = 'cell-border stripe',
                    caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: center;'
                      ,htmltools::em('Data Sources: NJ data from Envista, 
                                     Other states data from AirNow Tech')))
    }
    })
  ##################################################################
  ##################################################################
  ### Create info box to show what data is current to ###
  output$date_show<-renderInfoBox({
    infoBox(as.character(max(data1()$Date,na.rm = TRUE)),
            color = "green",icon = icon("calendar"),title = "Current to:",width = 6)
  })
  ##################################################################
  ### Allow users to download data ###
  output$data_download <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if(input$nj_select == "Daily Max"){
      write.csv(data, file)}
      else if (input$nj_select == "8 Hour Design Values"){
        write.csv(data_design,file)
      }
      else if (input$nj_select == "Highest Levels"){
        write.csv(highest_level,file)
        
      }
    }
  )
  ##################################################################
  ### Creates a pop window that only pops up when design value table is selected from dropdown, tells user that design values are preliminary ###
  observe({
    if(input$nj_select == "8 Hour Design Values"){
      showModal(modalDialog(
        title = "",
        div("Note: Design values are preliminary",style = "font-weight: bold;text-algin: center;
            font-size: 150%;")
        
      ))}
  })
  
}









