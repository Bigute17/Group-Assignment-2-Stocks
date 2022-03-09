library(shiny)
library(fpp3)
library(readr)

stocks <- read_csv('nyse_stocks.csv.zip')


ui <- fluidPage(
  textInput("text", label = h3("Stock Symbol"), value = "Enter Stock Symbol"),
  plotOutput('plot')
  
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    stocks %>% 
      filter(symbol ==   input$text ) %>% 
      autoplot(close) +
      labs(title =  input$text)
    
  })
  
}

shinyApp(ui, server)