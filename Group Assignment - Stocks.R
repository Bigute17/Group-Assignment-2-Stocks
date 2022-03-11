library(shiny)
library(fpp3)
library(readr)

# Read in and Convert to tsibble()
stocks <- read_csv('nyse_stocks.csv.zip')
stocks$date <- as.Date(stocks$date)
stocks <- tsibble(stocks, index = date, key = symbol)


ui <- fluidPage(
  numericInput("investment", label = h4("What if I had invested ___ dollars?"), 
               value = 1),
  
  textInput("text", label = h4("Input desired stock symbol"), 
  value = "AAPL"),hr(),fluidRow(column(3, verbatimTextOutput("value"))),
  
  plotOutput("forecast")
  )


server <- function(input, output, session) {
  
    output$forecast <- renderPlot({
      stocks %>%
      filter(symbol == input$text)%>%
      autoplot(close) +
      labs(title = input$text)})
    
    output$value <- renderPrint({ 
      filtered <- stocks %>% 
        filter(symbol == input$text) %>% 
        arrange(date)
      
      first_val <- head(filtered$close, 1)
      tail_val <- tail(filtered$close, 1)
      change <- tail_val - first_val
      percent_change <- change / first_val
      earnings <- input$investment * percent_change
      print(input$investment + earnings)
      })
    
    
}

shinyApp(ui, server)