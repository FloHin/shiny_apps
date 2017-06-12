library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cyborg"),
  
  # Application title
  titlePanel("Exercise 6 - Explore state.x77 (Murder)"),

   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       checkboxGroupInput("dimension", "Variablen für Model",
                          c("Population" = "Population",
                            "Einkommen" = "Income",
                            "Analphabetismus" = "Illiteracy",
                            "Lebenserwartung" = "Life_Exp",
                            "Hochschulabschluss" = "HS_Grad",
                            "Frost" = "Frost",
                            "Area" = "Area"),
                          c("Population",
                            "Income",
                            "Illiteracy",
                            "Life_Exp",
                            "HS_Grad",
                            "Frost",
                            "Area")),
       hr(),
       uiOutput("choose_income"),
       hr(),
       uiOutput("choose_columns")
       
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       tabsetPanel(
         tabPanel("Matrix", plotOutput("matrixcor"), verbatimTextOutput("selectedcor")),
         tabPanel("Data", tableOutput("data")),
         tabPanel("Summary", verbatimTextOutput("selectedsummary")),
         tabPanel("Residuals vs. Fitted", plotOutput("diagnosticPlot1")),
         tabPanel("Normal QQ", plotOutput("diagnosticPlot2")),
         tabPanel("Scale-Location", plotOutput("diagnosticPlot3")),
         tabPanel("Residuals vs. Leverage", plotOutput("diagnosticPlot4"))
       )
     )
   )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Funktion definieren
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
  
  #Basisdaten definieren (Für UI)
  state77ui <- as.data.frame(state.x77)
  
  #Columns renamen (isse dann einfacher zum handeln)
  colnames(state77ui)[colnames(state77ui)=="Life Exp"] <- "Life_Exp"
  colnames(state77ui)[colnames(state77ui)=="HS Grad"] <- "HS_Grad"
  
  #Datensatz erzeugen und filtern => Wird überall eingebunden
  state77dataset <- reactive({
    temp <- as.data.frame(state.x77)
    colnames(temp)[colnames(temp)=="Life Exp"] <- "Life_Exp"
    colnames(temp)[colnames(temp)=="HS Grad"] <- "HS_Grad"
    temp_filtered1 <- temp[c(input$datasetrows),]
    temp_filtered1[(temp_filtered1$Income <= input$maxincome),]
  })
  
  #UI - Check boxes für alle Datensätze
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(state77ui))
      return()
    
    # Get the data set with the appropriate name
    rownames <- rownames(state77ui)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("datasetrows", "Datensätze", 
                       choices  = rownames,
                       selected = rownames)
  })
  
  #UI - Eingabebalken für Einkommen rendern
  output$choose_income <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(state77ui))
      return()

    #Slider fürs Einkommen mit Maxwert vom aktuellen Datensatz
    sliderInput("maxincome", "Maximales Einkommen:", 
                min=min(state77ui[c(input$datasetrows),"Income"]), 
                max=max(state77ui[c(input$datasetrows),"Income"]), 
                value=max(state77ui[c(input$datasetrows),"Income"]))
  })
  
  #Plot für Pairs
  output$matrixcor <- renderPlot({
    #pairs(state77dataset(), lower.panel = panel.smooth, upper.panel = panel.cor)
    pairs(state77dataset()[, c("Murder", input$dimension)], lower.panel = panel.smooth, upper.panel = panel.cor)
  })

  #Datentabelle anzeigen
  output$data <- renderTable({
    state77dataset()[, c("Murder", input$dimension), drop = FALSE]
  }, rownames = TRUE)
  
  #Summary anzeigen
  output$selectedsummary <- renderPrint({
    if (length(input$dimension) > 0) {
      formula_string <- paste("Murder", " ~ ", paste(input$dimension, collapse="+"))
      model <- lm(as.formula(formula_string), state77dataset())
    summary(model)
    }
  })
  
  #Correlation anzeigen
  output$selectedcor <- renderPrint({
    #cor(state77[c(input$datasetrows),])
    cor(state77dataset()[, c("Murder", input$dimension)])
  })
  
  #Residuals vs. Fitted
  output$diagnosticPlot1 <- renderPlot({
    if (length(input$dimension) > 0) {
      formula_string <- paste("Murder", " ~ ", paste(input$dimension, collapse="+"))
      model <- lm(as.formula(formula_string), state77dataset())
      plot(model, 1)
    }
  }
  )
  
  #Normal QQ
  output$diagnosticPlot2 <- renderPlot({
    if (length(input$dimension) > 0) {
      formula_string <- paste("Murder", " ~ ", paste(input$dimension, collapse="+"))
      model <- lm(as.formula(formula_string), state77dataset())
      plot(model, 2)
    }
  }
  )
  
  #Scale-Location
  output$diagnosticPlot3 <- renderPlot({
    if (length(input$dimension) > 0) {
      formula_string <- paste("Murder", " ~ ", paste(input$dimension, collapse="+"))
      model <- lm(as.formula(formula_string), state77dataset())
      plot(model, 3)
    }
  }
  )
  
  #Residuals vs. Leverage
  output$diagnosticPlot4 <- renderPlot({
    if (length(input$dimension) > 0) {
      formula_string <- paste("Murder", " ~ ", paste(input$dimension, collapse="+"))
      model <- lm(as.formula(formula_string), state77dataset())
      plot(model, 5)
    }
  }
  )
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
