library(MASS)
library(e1071)
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

doRenderVariables <- function(index) {
  return(index != 0)
}

#' An helper function that returns if the ourput should be rendered.
#' 
#' @param index The index of first selected variable
#' @param index2 The index of second selected variable
#' @return Returns TRUE if the output should be rendered; otherwise FALSE
#' 
doRender <- function(index, index2) {
  return((doRenderVariables(index)) && (doRenderVariables(index2)) && (index != index2))
}

#' Renders the stats string for the selected variables name.
#' 
#' @param name The name from the swiss dataset to render
#' @param doRender If this plot should be rendered; by default TRUE
#' 
renderStatsName <- function(name, doRender = TRUE) {
  if (doRender) {
    paste("<h4>Stats of '", name, "'</h4>", sep = "")
  }
}

#' Renders a summary of selected data as specified by given index.
#' 
#' @param index The index from swiss dataset to render
#' @param doRender If this plot should be rendered; by default TRUE
#' 
renderSummary <- function(index, doRender = TRUE) {
  if (doRender) {
    data <- swiss[, index]
    summary(data)
  }
}

#' Renders metadata of selected variable as specified by given index.
#' 
#' @param index The index from swiss dataset to render
#' @param doRender If this plot should be rendered; by default TRUE
#' 
renderMetaData <- function(index, doRender = TRUE) {
  if (doRender) {
    data <- swiss[, index]
    standardDeviation <- round(sd(data), 3)
    mad <- round(mad(data), 3)
    skewnessFactor <- round(skewness(data)[[1]], 3)
    kurtosisFactor <- round(kurtosis(data)[[1]], 3)
    normalFactor = round(fitdistr(data, "normal")$loglik, 3)
    logFactor = round(fitdistr(data, "lognormal")$loglik, 3)
    
    # find text of skewness
    if (skewnessFactor > 0) {
      skewnessText = "right tailed"
    } else {
      skewnessText = "left tailed"
    }
    
    # find text of kurtosis
    if (kurtosisFactor > 0) {
      kurtosisText = "steilgipflig"
    } else if (kurtosisFactor == 0) {
      kurtosisText = "normalgipflig"
    } else {
      kurtosisText = "flachgipflig"
    }
    
    paste0(
      "Stand. Dev.: ", standardDeviation, "\n",
      "MAD: ", mad, "\n",
      "Skewness: ", skewnessFactor, " -> ", paste0(skewnessText), "\n",
      "Kusrtosis: ", kurtosisFactor, " -> ", paste0(kurtosisText), "\n",
      "Normal Distribution Factor: ", normalFactor, "\n",
      "Logarithmic Distribution Factor: ", logFactor
    )
  }
}

#' Renders a Distribution Plot.
#' 
#' @param index The index from swiss dataset to render
#' @param name The name from the swiss dataset to render
#' @param doRender If this plot should be rendered; by default TRUE
#'
renderDistribution <- function(index, name, doRender = TRUE) {
  if (doRender) {
    renderPlot({
      data <- swiss[, index]
      dataSorted <- seq(from = min(data), to = max(data), length = length(data))
      y = dnorm(dataSorted, mean = mean(data), sd = sd(data))
      plot(density(data), main = paste("Distribution of ", name));
      lines(dataSorted, y, col = "red")
    })
  }
}

#' Renders a Histrogram.
#' 
#' @param index The index from swiss dataset to render
#' @param name The name from the swiss dataset to render
#' @param doRender If this plot should be rendered; by default TRUE
#'
renderHistogram <- function(index, name, bins = 30, doRender = TRUE) {
  if (doRender) {
    renderPlot({
      data <- swiss[, index]
      binsSeq <- seq(min(data), max(data), length.out = bins + 1)
      hist(data, breaks = binsSeq, main = paste("Histogram of ", name), xlab = "Value")
    })
  }
}

#' Renders a Box Plot.
#' 
#' @param index The index from swiss dataset to render
#' @param name The name from the swiss dataset to render
#' @param bins The bins for the histogram; by default 30
#' @param doRender If this plot should be rendered; by default TRUE
#'
renderBoxPlot <- function(index, name, bins = 30, doRender = TRUE) {
  if (doRender) {
    renderPlot({
      data <- swiss[, index]
      boxplot(data, horizontal = TRUE, main = paste("Boxplot of ", name))
    })
  }
}

#' Renders a Violin Plot.
#' 
#' @param index The index from swiss dataset to render
#' @param name The name from the swiss dataset to render
#' @param doRender If this plot should be rendered; by default TRUE
#'
renderViolinPlot <- function(index, name, doRender = TRUE) {
  if (doRender) {
    renderPlot({
      data <- swiss[, index]
      vioplot(data, horizontal = TRUE)
    })
  }
}

#' Renders a QQ Plot.
#' 
#' @param index The index from swiss dataset to render
#' @param name The name from the swiss dataset to render
#' @param doRender If this plot should be rendered; by default TRUE
#'
renderQQPlot <- function(index, name, doRender = TRUE) {
  if (doRender) {
    renderPlot({
      data <- swiss[, index]
      qqnorm(data, main = paste("QQ Plot of ", name))
      qqline(data, col = 'red')
    })
  }
}

#' Renders a Scatter Plot.
#' 
#' @param index The index from swiss dataset to render
#' @param name The name from the swiss dataset to render
#' @param doRender If this plot should be rendered; by default TRUE
#'
renderScatterPlot <- function(index, name, doRender = TRUE) {
  if (doRender) {
    renderPlot({
      data <- swiss[, index]
      plot(data, main = paste("Scatterplot of ", name))
    })
  }
}

#' Renders a Scatter Plot that represents the data as specified by given indexes and names, to show
#' releation between them.
#' 
#' @param index The index from swiss dataset to render
#' @param name The name from the swiss dataset to render
#' @param index2 The index from swiss dataset to render
#' @param name2 The name from the swiss dataset to render
#' @param doRender If this plot should be rendered; by default TRUE
#' 
renderScatterPlotVs <- function(index, name, index2, name2, doRender = TRUE) {
  if (doRender) {
    renderPlot({
      data = swiss[, index]
      data2 = swiss[, index2]
      corr = round(cor(data, data2), 4)
      
      plot(data, data2, xlab = name, ylab = name2)
      abline(lm(data2 ~ data), col="red")
      lines(lowess(data, data2), col="blue")
      title(paste(name, " vs. ", name2, ": ", corr, sep = ""))
    })
  }
}

#' Defines the initial function which is called by the shiny app backend server.
#'
#' @param input The input object from shiny app frontend for this backend server
#' @param output The output object to the shiny app frontend from this backend server
#' @return This function returns void as the shiny server back ignores all returns from it.
#'   However, the given output variable is described here in more detail, as this object's
#'   fields are required for the frontend (UI) to work successfully. The following fields
#'   in the output object can be defined:
#'   \describe{
#'     \item{output$ui} The whole main panel UI for the frontend to render
init <- function(input, output) {

  # create reactive functions to parse input variables
  selectedVar <- reactive({
    selectedVar <- strsplit(input$selectedVar, ",")[[1]] # list("1","Fertility")
  })
  selectedVar2 <- reactive({
    selectedVar2 <- strsplit(input$selectedVar2, ",")[[1]] # list("1","Fertility")
  })
  
  # generate summary outputs for first selected variable
  output$statsName1 <- renderText({
    index = strtoi(selectedVar()[1])
    name <- selectedVar()[2]
    render <- doRenderVariables(index)
    renderStatsName(name, doRender = render)
  })
  output$statsName2 <- renderText({
    index = strtoi(selectedVar2()[1])
    name <- selectedVar2()[2]
    render <- doRenderVariables(index)
    renderStatsName(name, doRender = render)
  })
  
  # generate summary outputs for selected variables
  output$summary1 <- renderPrint({
    index = strtoi(selectedVar()[1])
    render <- doRenderVariables(index)
    renderSummary(index, doRender = render)
  })
  output$summary2 <- renderPrint({
    index = strtoi(selectedVar2()[1])
    render <- doRenderVariables(index)
    renderSummary(index, doRender = render)
  })
  
  # generate metadata outputs for selected variables
  output$meta1 <- renderText({
    index = strtoi(selectedVar()[1])
    render <- doRenderVariables(index)
    renderMetaData(index, doRender = render)
  })
  output$meta2 <- renderText({
    index = strtoi(selectedVar2()[1])
    render <- doRenderVariables(index)
    renderMetaData(index, doRender = render)
  })
  
  # render UI based on selected variables
  output$ui <- renderUI({
    index <- strtoi(selectedVar()[1])
    name <- selectedVar()[2]
    
    index2 <- strtoi(selectedVar2()[1])
    name2 <- selectedVar2()[2]
    
    bins <- input$bins
    render2nd <- doRender(index, index2)
    
    if (index == 0) {
      tabsetPanel(
        tabPanel("Scatteplot Matrix", renderPlot({
          pairs(swiss, lower.panel = panel.smooth, upper.panel = corFunction)
        }))
      )
    } else {
      tabsetPanel(id = "selectedTab", selected = input$selectedTab,
        tabPanel("Distribution",
          renderDistribution(index, name),
          renderDistribution(index2, name2, doRender = render2nd)
        ),
        tabPanel("Histogram",
          renderHistogram(index, name, bins = bins),
          renderHistogram(index2, name2, bins = bins, doRender = render2nd)
        ),
        tabPanel("Box Plot",
          renderBoxPlot(index, name),
          renderBoxPlot(index2, name2, doRender = render2nd)
        ),
        tabPanel("Violin Plot",
          renderViolinPlot(index, name),
          renderViolinPlot(index2, name2, doRender = render2nd)
        ),
        tabPanel("QQ Plot",
          renderQQPlot(index, name),
          renderQQPlot(index2, name2, doRender = render2nd)
        ),
        tabPanel("Scatterplot",
          renderScatterPlot(index, name),
          renderScatterPlot(index2, name2, doRender = render2nd),
          renderScatterPlotVs(index, name, index2, name2, doRender = render2nd)
        )
      )
    }
  })
}

# initialize shiny app server
shinyServer(init)
