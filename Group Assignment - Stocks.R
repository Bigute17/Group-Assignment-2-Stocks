library(shiny)
library(fpp3)
library(readr)

stocks <- read_csv('nyse_stocks.csv.zip')
stocks$date <- as.Date(stocks$date)
stocks <- tsibble(stocks, index = date, key = symbol)

ui <- fluidPage(
  textInput("text", label = h3("Stock Symbol"), value = "AAPL"),
  radioButtons(
    inputId = 'selected_col',
    label = 'What would you like to plot?',
    choices = c('open', 'close',
                'high', 'low', 'volume')),
  plotOutput('plot')
  
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    stocks %>% 
      filter(symbol == input$text ) %>% 
      autoplot(input$selected_col) +
      labs(title =  input$text)
  })
  
}

shinyApp(ui, server)