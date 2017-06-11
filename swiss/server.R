library(shiny)
library(utils)
library(vioplot)

#' Source an other file in the same directory.
#'
#' @param x A file to source
#' @param ... Some other files to source
sourceHere <- function(x, ...) {
  dir <- "."
  if (sys.nframe() > 0) {
    frame <- sys.frame(1)
    if (!is.null(frame$ofile)) {
      dir <- dirname(frame$ofile)
    }
  }
  source(file.path(dir, x), ...)
}

# Source all required files in same directory
sourceHere('corFunction.R')

#' Renders distribution plots.
#' 
#' @param output The output from the server
#' @param selectedVar The first variable that was selected at the UI
#' @param selectedVar2 The second variable that was selected at the UI
#' 
renderDistribution <- function(output, selectedVar, selectedVar2) {
  output$distribution <- renderPlot({
    index <- strtoi(selectedVar()[1]) 
    name <- selectedVar()[2]
    data <- swiss[, index]
  
    dataSorted <- seq(from = min(data), to = max(data), length = length(data))
    y = dnorm(dataSorted, mean = mean(data), sd = sd(data))
    plot(density(data), main = name);
    lines(dataSorted, y, col = "red")
  })
  output$distribution2 <- renderPlot({
    if (!identical(strtoi(selectedVar()[1]), strtoi(selectedVar2()[1]))) {
      index <- strtoi(selectedVar2()[1])
      name <- selectedVar2()[2]
      data <- swiss[, index]
      
      dataSorted <- seq(from = min(data), to = max(data), length = length(data))
      y = dnorm(dataSorted, mean = mean(data), sd = sd(data))
      plot(density(data), main = name);
      lines(dataSorted, y, col = "red")
    }
  })
}

#' Renders histogram plots.
#' 
#' @param output The output from the server
#' @param selectedVar The first variable that was selected at the UI
#' @param selectedVar2 The second variable that was selected at the UI
#' @param selectedBins The number bins for the histogram
#' 
renderHistogram <- function(output, selectedVar, selectedVar2, selectedBins) {
  output$histogram <- renderPlot({
    print(selectedBins)
    index <- strtoi(selectedVar()[1])
    name <- selectedVar()[2]
    data <- swiss[, index]
    
    bins <- seq(min(data), max(data), length.out = selectedBins + 1)
    hist(data, breaks = bins, xlab = name)
  })
  output$histogram2 <- renderPlot({
    if (!identical(strtoi(selectedVar()[1]), strtoi(selectedVar2()[1]))) {
      index <- strtoi(selectedVar2()[1])
      name <- selectedVar2()[2]
      data <- swiss[, index]
      
      bins <- seq(min(data), max(data), length.out = selectedBins + 1)
      hist(data, breaks = bins, xlab = name)
    }
  })
}

#' Renders box plots.
#' 
#' @param output The output from the server
#' @param selectedVar The first variable that was selected at the UI
#' @param selectedVar2 The second variable that was selected at the UI
#' 
renderBoxPlot <- function(output, selectedVar, selectedVar2) {
  output$boxPlot <- renderPlot({
    index <- strtoi(selectedVar()[1])
    name <- selectedVar()[2]
    data <- swiss[, index]
    
    boxplot(data, horizontal = TRUE)
  })
  output$boxPlot2 <- renderPlot({
    if (!identical(strtoi(selectedVar()[1]), strtoi(selectedVar2()[1]))) {
      index <- strtoi(selectedVar2()[1])
      name <- selectedVar2()[2]
      data <- swiss[, index]
      
      boxplot(data, horizontal = TRUE)
    }
  })
}

#' Renders violin plots.
#' 
#' @param output The output from the server
#' @param selectedVar The first variable that was selected at the UI
#' @param selectedVar2 The second variable that was selected at the UI
#' 
renderViolinPlot <- function(output, selectedVar, selectedVar2) {
  output$vioPlot <- renderPlot({
    index <- strtoi(selectedVar()[1])
    name <- selectedVar()[2]
    data <- swiss[, index]
    
    vioplot(data, horizontal = TRUE)
  })
  output$vioPlot2 <- renderPlot({
    if (!identical(strtoi(selectedVar()[1]), strtoi(selectedVar2()[1]))) {
      index <- strtoi(selectedVar2()[1])
      name <- selectedVar2()[2]
      data <- swiss[, index]
      
      vioplot(data, horizontal = TRUE)
    }
  })
}

#' Renders Q-Q plots.
#' 
#' @param output The output from the server
#' @param selectedVar The first variable that was selected at the UI
#' @param selectedVar2 The second variable that was selected at the UI
#' 
renderQQPlot <- function(output, selectedVar, selectedVar2) {
  output$qqPlot <- renderPlot({
    index <- strtoi(selectedVar()[1])
    name <- selectedVar()[2]
    data <- swiss[, index]
    
    qqnorm(data, main = name)
    qqline(data, col = 'red')
  })
  output$qqPlot2 <- renderPlot({
    if (!identical(strtoi(selectedVar()[1]), strtoi(selectedVar2()[1]))) {
      index <- strtoi(selectedVar2()[1])
      name <- selectedVar2()[2]
      data <- swiss[, index]
      
      qqnorm(data, main = name)
      qqline(data, col = 'red')
    }
  })
}

#' Renders scatter plots.
#' 
#' @param output The output from the server
#' @param selectedVar The first variable that was selected at the UI
#' @param selectedVar2 The second variable that was selected at the UI
#' 
renderScatterPlot <- function(output, selectedVar, selectedVar2) {
  output$scatterPlot <- renderPlot({
    index <- strtoi(selectedVar()[1])
    name <- selectedVar()[2]
    data <- swiss[, index]
    
    plot(data)
  })
  output$scatterPlot2 <- renderPlot({
    if (!identical(strtoi(selectedVar()[1]), strtoi(selectedVar2()[1]))) {
      index <- strtoi(selectedVar2()[1])
      name <- selectedVar2()[2]
      data <- swiss[, index]
      
      plot(data)
    }
  })
  output$scatterPlotMatrix <- renderPlot({
    pairs(swiss, lower.panel = panel.smooth, upper.panel = corFunction)
  })
}

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
  # this is only example code that can be removed
  output$renderedSelectdPredefinedFilter <-
    renderPrint({
      input$selectdPredefinedFilter
    })
  
  # reactive functions for parsin input
  selectedVar <- reactive(
    selectedVar <- strsplit(input$selectedVar, ",")[[1]]
  )
  selectedVar2 <- reactive(
    selectedVar2 <- strsplit(input$selectedVar2, ",")[[1]]
  )
  
  # print plots
  renderDistribution(output, selectedVar, selectedVar2)
  renderHistogram(output, selectedVar, selectedVar2, input$bins)
  renderBoxPlot(output, selectedVar, selectedVar2)
  renderViolinPlot(output, selectedVar, selectedVar2)
  renderQQPlot(output, selectedVar, selectedVar2)
  renderScatterPlot(output, selectedVar, selectedVar2)
}

# initialize shiny app server
shinyServer(init)
