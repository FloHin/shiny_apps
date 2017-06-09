library(shiny)
library(vioplot)

#' Source an other file in the same directory.
#' 
#' @param x A file to source
#' @param ... Some other files to source
sourceHere <- function(x, ...) {
  dir <- "."
  if(sys.nframe()>0) {
    frame <- sys.frame(1)
    if (!is.null(frame$ofile)) {
      dir <- dirname(frame$ofile)
    }
  }
  source(file.path(dir, x), ...)
}

# Source all required files in same directory
sourceHere('corFunction.R')

#' Defines the initial function which is called by the shiny app backend server.
#'
#' @param input The input object from shiny app frontend for this backend server
#' @param output The output object to the shiny app frontend from this backend server
#' @return This function returns void as the shiny server back ignores all returns from it.
#'   However, the given output variable is described here in more detail, as this object's
#'   fileds are required for the frontend (UI) to work successfully. The following fields
#'   in the output object can be defined:
#'   \describe{
#'     \item{output$distribution} The variable for some distribution
#'     \item{output$histogram} The variable for a histogram (i.e. hist(x, ...))
#'     \item{output$boxPlot} The varibale for a boxplot (i.e. boxplot(x, ...))
#'     \item{output$kernelDestinyEstimation} The variable for a Kernel Density Estimation function (i.e. density(x, ...))
#'     \item{output$vioPlot} The variable for a Violin Plot (i.e. vioplot(x, ...));
#'     \item{output$qqPlot} The variable for a Quantile-Quantile Plot (QQ Plot) (i.e. qqplot, qqnorm, qqline, ...)
#'     \item{output$scatterPlot} The variable for a Scatterplot or Scatterplot Matrix (i.e. plot(x, y, ...), pairs(x, ...))
#'     \item{output$mosaicPlot} The variable for a Mosaic Plot (i.e. mosaicplot(x, ...))
init <- function(input, output) {
  
  # example code
  output$scatterPlot <- renderPlot({
    pairs(swiss, lower.panel = panel.smooth, upper.panel = corFunction)
  })
  
  # this is only example code that can be removed
  output$renderedSelectdPredefinedFilter <- renderPrint({ input$selectdPredefinedFilter })
}

# initialize shiny app server
shinyServer(init)
