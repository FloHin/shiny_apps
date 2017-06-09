library(shiny)
library(vioplot)

ss <- as.data.frame(state.x77)
# panel.cor
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

chooser_options = list(
  "All" = 0,
  "Population" = 1,
  "Income" = 2,
  "Illiteracy" = 3,
  "Life.Exp" = 4,
  "Murder" = 5);

chooseData = function(chooser) {
  if (chooser != 0) {
    switch(as.character(chooser),
           "1"={x <- ss$Population},
           "2"={x <- ss$Income},
           "3"={x <- ss$Illiteracy},
           "4"={x <- ss$`Life Exp`},
           "5"={x <- ss$Murder},
           stop("stop?")
    )
    x;
  } else {
    c(0);
  }
}

chooseNorm = function(chooser, x, xfit) {
  N <- length(x);
  switch(as.character(chooser),
         "1"={y <- dnorm(xfit, mean=mean(x), sd=sd(x))},
         "2"={y <- dnorm(xfit, mean=mean(x), sd=sd(x))},
         "3"={y <- dnorm(xfit, mean=mean(x), sd=sd(x))},
         "4"={y <- dnorm(xfit, mean=mean(x), sd=sd(x))},
         "5"={y <- dnorm(xfit, mean=mean(x), sd=sd(x))},
         stop("stop?")
  )
  y;
  
}

server <- function(input, output) {
  
  selector<-reactive({as.numeric(input$chooser)});

  output$title <- renderText({ 
    paste("", chooser_options[input$chooser])
  })
  
  output$p1 <- renderPlot({
    if (input$chooser == 0) {
      plot(ss)
    }
    
    if (input$chooser != 0) {
      x <- chooseData(input$chooser);
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = 'skyblue', border = 'white')
      
      xfit<-seq(min(x),max(x),length=length(x)) 
      
      y = chooseNorm(input$chooser,x, xfit)
      print(density(x));
      plot(density(x));
      #lines(xfit,x,col='red')
      lines(xfit, y, col="red", lwd=1)
      
    }
  })
  output$p2 <- renderPlot({
    if (input$chooser != 0) {
      x <- chooseData(input$chooser);
      qqnorm(x);qqline(x);
    }
  })
  output$p3 <- renderPlot({
    if (input$chooser != 0) {
      x <- chooseData(input$chooser);
      boxplot(x, horizontal = TRUE)
    }
  })
  output$p4 <- renderPlot({
    if (input$chooser != 0) {
      x <- chooseData(input$chooser);
      qqnorm(x);qqline(x);
    }
  })
  
  
  
  output$distPlot1 <- renderPlot({
    #vioplot(x)
  })
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Swiss!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      
    radioButtons("chooser", label = h3("Choose variable"),
        choices = chooser_options,selected = 0),
    
      sliderInput("bins",
                  "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      fluidRow(
        column(12,
               h2(textOutput("title"))
        )
      ),
        fluidRow(
          
        column(6,
               plotOutput("p1")
        ),
        column(6,
               plotOutput("p2")
        ),
        column(6,
              plotOutput("p3")
        ),
        column(6,
               plotOutput("p4")
        )
        
      )
      
      
      )
  )
)

shinyApp(ui = ui, server = server)