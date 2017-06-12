library(shiny)
library(utils)
library(vioplot)

#' Setup the sidebar content.
#'
#' @param input The server input
#' @param output The server output
#' @return One or more sidebar panel compatible objects that will be used for building up the sidebar
#' @seealso \link{sidebarPanel}
#'
setupSidebarContent <- function(input, output) {
  # prepare variables of swiss dataset for selection
  vars1st <- list()
  vars1st[["All"]] <- paste0(0, ",", "All")
  for (i in 1:length(colnames(swiss))) {
    key <- colnames(swiss)[i]
    value <- paste0(i, ",", key)
    vars1st[[key]] <- value
  }
  vars2nd <- list()
  vars2nd[["None"]] <- paste0(0, ",", "None")
  for (i in 1:length(colnames(swiss))) {
    key <- colnames(swiss)[i]
    value <- paste0(i, ",", key)
    vars2nd[[key]] <- value
  }
  
  # return sidebar UI controls
  return(list(
    radioButtons("selectedVar", label = "Select 1st variable:", choices = vars1st),
    radioButtons("selectedVar2", label = "Select 2nd variable:", choices = vars2nd),
    sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
  ))
}

# define UI for application that draws a histogram
shinyUI(fluidPage(
  # application title
  titlePanel("Exercise 1: Swiss"),
  
  # sidebar panel
  sidebarLayout(
    sidebarPanel(
      setupSidebarContent(input, output),
      htmlOutput("statsName1"),
      verbatimTextOutput("summary1"),
      verbatimTextOutput("meta1"),
      htmlOutput("statsName2"),
      verbatimTextOutput("summary2"),
      verbatimTextOutput("meta2")
    ),
    
    # main panel
    mainPanel(
      uiOutput("ui")
    )
  )
))
