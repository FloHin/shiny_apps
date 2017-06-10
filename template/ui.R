library(shiny)
library(vioplot)

#' Setup the sidebar content.
#'
#' @param input The server input
#' @param output The server output
#' @return One or more sidebar panel compatible objects that will be used for building up the sidebar
#' @seealso \link{sidebarPanel}
#'
setupSidebarContent <- function(input, output) {
  # add some content to build up sidebar; below is only example code that can be removed
  return(list(
    fluidRow(
      column(3, verbatimTextOutput("renderedSelectdPredefinedFilter"))
    )
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
  titlePanel("Old Faithful Geyser Data"),
  
  # sidebar panel
  sidebarLayout(
    sidebarPanel(
      setupSidebarContent(input, output),
      selectInput(
        "selectdPredefinedFilter",
        label = h3("Select some predefined filter ..."),
        choices = setupPredefinedFilters(input, output)
      )
    ),
    
    # main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Distribution", plotOutput("distribution")),
        tabPanel("Histogram", plotOutput("histogram")),
        tabPanel("Box Plot", plotOutput("boxPlot")),
        tabPanel("Kernel Density Estimation", plotOutput("kernelDestinyEstimation")),
        tabPanel("Violin Plot", plotOutput("vioPlot")),
        tabPanel("QQ Plot", plotOutput("qqPlot")),
        tabPanel("Scatterplot", plotOutput("scatterPlot")),
        tabPanel("Mosaic Plot", plotOutput("mosaicPlot"))
      )
    )
  )
))
