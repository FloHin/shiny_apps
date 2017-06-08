library(shiny)
library(vioplot)

# swiss

server <- function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    x    <- swiss$Fertility
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    qqnorm(x)
  })
  output$distPlot1 <- renderPlot({
    x    <- swiss$Agriculture
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'skyblue', border = 'white')
    vioplot(x)
    
  })
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Swiss!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Fertility"),
      plotOutput("distPlot"),
      h2("Agriculture"),
      plotOutput("distPlot1")
      
      
      )
  )
)

shinyApp(ui = ui, server = server)