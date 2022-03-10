library(shiny)
library(fpp3)
library(readr)
library(shinyWidgets)

stocks <- read_csv('nyse_stocks.csv.zip')
stocks$date <- as.Date(stocks$date)
stocks <- tsibble(stocks, index = date, key = symbol)
for (i in 1:length(unique(stocks$symbol))) {
  stocks$totalgrowth[i] <- stocks$close[stocks$symbol == stocks$symbol[i] & stocks$date == "2016-12-30"] 
  - stocks$open[stocks$symbol == stocks$symbol[i] & stocks$date == "2010-01-04"] }

ui <- fluidPage(
  pickerInput(
    inputId = "stock",
    label = "Pick one or more stocks.", 
    choices = unique(stocks$symbol),
    options = list(
      `live-search` = TRUE), multiple = TRUE
  ),
  radioButtons(
    inputId = 'selected_col',
    label = 'What would you like to plot?',
    choices = c('open', 'close',
                'high', 'low', 'volume')),
  plotOutput('plot')
  
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    stocks[, c('symbol', 'date', input$selected_col)] %>% 
      filter(symbol == input$stock ) %>% 
      autoplot() +
      labs(title = input$stock)
  })
  
}

shinyApp(ui, server)