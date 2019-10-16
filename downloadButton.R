### This script creates a shiny module that creates a download button for shiny apps and then allows users to download that data ###
### Kevin Zolea ; 10/2019 ###

downloadObjUI <- function(id) {
  ns <- NS(id)
  
  downloadButton(ns("data_download"), label = "Download Data", class = "btn-primary")
}

downloadObj <- function(input, output, session, data) {
  
  output$data_download <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file) # add parentheses to data arg if reactive
    }
  )
}
