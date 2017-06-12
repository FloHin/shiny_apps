library(shiny)
library(vioplot)
library(moments)
library(MASS)

ss <- as.data.frame(state.x77)

corFn <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

chooser_names = list("All",
                     "Population",
                     "Income",
                     "Illiteracy",
                     "Life.Exp" ,
                     "Murder")

chooser_options = list(
  "All" = 0,
  "Population" = 1,
  "Income" = 2,
  "Illiteracy" = 3,
  "Life.Exp" = 4,
  "Murder" = 5
)


chooser2_options = list(
  "None" = 0,
  "Population" = 1,
  "Income" = 2,
  "Illiteracy" = 3,
  "Life.Exp" = 4,
  "Murder" = 5
)


chooseData = function(chooser) {
  if (chooser != 0) {
    switch(
      as.character(chooser),
      "1" = {
        x <- ss$Population
      },
      "2" = {
        x <- ss$Income
      },
      "3" = {
        x <- ss$Illiteracy
      },
      "4" = {
        x <- ss$`Life Exp`
      },
      "5" = {
        x <- ss$Murder
      },
      stop("stop?")
    )
    x
    
  } else {
    c(0)
    
  }
}

chooseNorm = function(chooser, x, xfit) {
  N <- length(x)
  
  switch(
    as.character(chooser),
    "1" = {
      y <- dnorm(xfit, mean = mean(x), sd = sd(x))
    },
    "2" = {
      y <- dnorm(xfit, mean = mean(x), sd = sd(x))
    },
    "3" = {
      y <- dnorm(xfit, mean = mean(x), sd = sd(x))
    },
    "4" = {
      y <- dnorm(xfit, mean = mean(x), sd = sd(x))
    },
    "5" = {
      y <- dnorm(xfit, mean = mean(x), sd = sd(x))
    },
    stop("stop?")
  )
  y
  
  
}

textList = function(default, i) {
  if (i == 0) {
    default
  } else {
    chooser_names[[1 + as.numeric(i)]]
  }
}

clab = function(i) {
  textList("", i)
}

textVersus = function(i1, i2) {
  if (i1 == 0 || i2 == 0) {
    ""
  } else {
    paste(textList("All", i1), textList("All", i2), sep = " vs ")
  }
}

server <- function(input, output) {
  selector <- reactive({
    as.numeric(input$chooser)
  })
  
  output$id <- renderText({
    textList("All", input$chooser)
  })
  
  output$title <- renderText({
    textList("All", input$chooser)
  })
  
  output$versus <- renderText({
    textVersus(input$chooser, input$chooser2)
    
  })
  output$summary <- renderPrint({
    if (input$chooser != 0) {
      x <- chooseData(input$chooser);
      summary(x);
    }
  })
  output$summary2 <- renderText({
    if (input$chooser != 0) {
      x <- chooseData(input$chooser);
      
      fnb = try(round(fitdistr(x, "negative binomial")$loglik,3), TRUE)
      fn = fitdistr(x, "normal")$loglik
      fln = fitdistr(x, "lognormal")$loglik
      if (!is.numeric(fnb)) fnb = "-_-"
      as.character(paste0(
      " Skew.: ",round(skewness(x)[[1]],3),
      " Kurt.: ",round(kurtosis(x)[[1]],3),'\n',
      " std. dev.: ",round(sd(x),3),'\n',      
      " Mad: ",round(mad(x),3),'\n',      
      "Fitting to distributions: \n",
      " normal: ",round(fn,3),
      " lognorm: ",round(fln,3),
      " neg. bin.: ",fnb
      )[[1]])
    }
  })
  output$p1 <- renderPlot({
    if (input$chooser == 0) {
     pairs(ss[1:5], lower.panel = panel.smooth, upper.panel = corFn)
    }
    
    if (input$chooser != 0) {
      x <- chooseData(input$chooser)
      
      xfit <- seq(min(x), max(x), length = length(x))
      
      y = chooseNorm(input$chooser, x, xfit)
      #print(density(x))
      
      plot(density(x),
           main = paste(textList("", input$chooser), " norm distribution", sep = " vs "))
      
      #lines(xfit,x,col='red')
      lines(xfit, y, col = "red", lwd = 1)
      #title();
    }
  })
  output$p2 <- renderPlot({
    if (input$chooser != 0) {
      x <- chooseData(input$chooser)
      qqnorm(x)
      qqline(x)
    }
  })
  output$p3 <- renderPlot({
    if (input$chooser != 0) {
      x <- chooseData(input$chooser)
      
      boxplot(x, horizontal = TRUE)
      title("Boxplot")
    }
  })

  output$p4 <- renderPlot({
    if (input$chooser != 0 && input$chooser2 != 0) {
      x <- chooseData(input$chooser)
      
      y <- chooseData(input$chooser2)
      
      plot(x,
           y,
           xlab = clab(input$chooser),
           ylab = clab(input$chooser2))
      
      abline(lm(y ~ x), col = "red") # regression line (y~x)
      lines(lowess(x, y), col = "blue") # lowess line (x,y)
      corr = round(cor(x, y), 4)
      
      title(paste(textVersus(input$chooser, input$chooser2), corr, sep = " "))
    }
  })
  
  output$distPlot1 <- renderPlot({
    #vioplot(x)
  })
}

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  
  #id <- textOutput("id"),
  #print(id),
  #is_all <- grepl(id,"All"),
  fluidRow(
    column(
      3,
      # Sidebar with a slider input for the number of bins
      
      titlePanel("UE2 states.x77"),
      
      radioButtons(
        "chooser",
        label = h3("Choose variable"),
        choices = chooser_options,
        selected = 0
      ),
      
      radioButtons(
        "chooser2",
        label = h3("Choose comparison"),
        choices = chooser2_options,
        selected = 0
      )
      

    ),
    
    # Show a plot of the generated distribution
    column(9,
           
           fluidRow(column(12,
                           h2(
                             textOutput("title")
                           ))),
           fluidRow(column(6,
                           verbatimTextOutput("summary")
                    ),
                    column(6, 
                           verbatimTextOutput("summary2")
                    )),
           
           if (grepl("All",textOutput("id"))) {
             fluidRow(column(12, plotOutput("p1")))
             
           }  else {
             fluidRow(
               column(6,
                      plotOutput("p1")),
               column(6,
                      plotOutput("p2")),
               column(6,
                      plotOutput("p3")),
               column(6,
                      plotOutput("p4"))
             )
             
           }
           
           # column, fluidRow, fluidPage/ui
  )
))
  
  
  shinyApp(ui = ui, server = server)