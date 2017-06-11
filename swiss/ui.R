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
  # prepare variables for swiss dataset
  vars <- list()
  for (i in 1:length(colnames(swiss))) {
    key <- colnames(swiss)[i]
    value <- paste0(i, ",", key)
    vars[[key]] <- value
  }
  
  # return sidebar UI controls
  return(list(
    radioButtons("selectedVar", label = "Select 1st variable:", choices = vars),
    radioButtons("selectedVar2", label = "Select 2nd variable:", choices = vars),
    sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
  ))
}

#' Setup predefined filter objects.
#'
#' @param input The server input
#' @param output The server output
#' @return A list with predefined filter objects that will be used to send a predefined filter to the server
#'
setupPredefinedFilters <- function(input, output) {
  # add some list content for select field; below is only example code that can be removed
  return(list(
    "Example1" = 1,
    "Example2" = 2)
  )
}

# define UI for application that draws a histogram
shinyUI(fluidPage(
  # application title
  titlePanel("Exercise 1: Swiss"),
  
  # sidebar panel
  sidebarLayout(
    sidebarPanel(
      setupSidebarContent(input, output),
      selectInput(
        "selectdPredefinedFilter",
        label = "Select some predefined filter ...",
        choices = setupPredefinedFilters(input, output)
      )
    ),
    
    # main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Distribution", 
           plotOutput("distribution"),
           plotOutput("distribution2")
        ),
        tabPanel("Histogram", 
          plotOutput("histogram"),
          plotOutput("histogram2")
        ),
        tabPanel("Box Plot", 
          plotOutput("boxPlot"),
          plotOutput("boxPlot2")
        ),
        tabPanel("Violin Plot", 
          plotOutput("vioPlot"),
          plotOutput("vioPlot2")
        ),
        tabPanel("QQ Plot", 
          plotOutput("qqPlot"),
          plotOutput("qqPlot2")
        ),
        tabPanel("Scatterplot", 
          plotOutput("scatterPlot"),
          plotOutput("scatterPlot2")
        ),
        tabPanel("Scatterplot Matrix", 
          plotOutput("scatterPlotMatrix")
        )
      )
    )
  )
))
