library(shiny)
library(vioplot)

ss <- as.data.frame(state.x77)


chooser_names = list(
  "All",
  "Population",
  "Income",
  "Illiteracy",
  "Life.Exp" ,
  "Murder");
chooser_options = list(
  "All" = 0,
  "Population" = 1,
  "Income" = 2,
  "Illiteracy" = 3,
  "Life.Exp" = 4,
  "Murder" = 5);

chooser2_options = list(
  "None" = 0,
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

textList = function(default, i) {
  if (i == 0) {
    default;
  } else {
    chooser_names[[1+as.numeric(i)]]
  }
}

clab = function(i) {
  textList("",i);
}

textVersus = function(i1, i2) {
  if (i1 == 0 || i2 == 0) {
    ""
  } else {
    paste(textList("All",i1), textList("All",i2), sep = " vs ");
    
  }
}

server <- function(input, output) {
  
  selector<-reactive({as.numeric(input$chooser)});

  output$title <- renderText({ 
    textList("All",input$chooser);

    #paste("", chooser_options[input$chooser])
  });
  
  output$versus <- renderText({
    textVersus(input$chooser, input$chooser2);
  });
  
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
      plot(density(x), main = paste(textList("",input$chooser), " density", sep=" vs "));
      #lines(xfit,x,col='red')
      lines(xfit, y, col="red", lwd=1)
      #title();
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
      boxplot(x, horizontal = TRUE);
      title("Boxplot");
    }
  })
  output$p4 <- renderPlot({
    if (input$chooser != 0 && input$chooser2 != 0 ) {
      x <- chooseData(input$chooser);
      y <- chooseData(input$chooser2);
      
      plot(x,y, xlab = clab(input$chooser), ylab = clab(input$chooser2));
      title(textVersus(input$chooser, input$chooser2));
    }
  })
  
  
  
  output$distPlot1 <- renderPlot({
    #vioplot(x)
  })
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      titlePanel("UE2 states.x77"),
      
    radioButtons("chooser", label = h3("Choose variable"),
        choices = chooser_options,selected = 0),
    
    radioButtons("chooser2", label = h3("Choose comparison"),
                 choices = chooser2_options,selected = 0),
    
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
               textOutput("versus"),
               plotOutput("p4")
        )
        
      )
      
      
      )
  )
)

shinyApp(ui = ui, server = server)