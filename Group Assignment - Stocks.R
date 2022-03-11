library(shiny)
library(fpp3)
library(readr)
# install.packages("shinyWidgets")
library(shinyWidgets)

# Read in and Convert to tsibble()
stocks <- read_csv('nyse_stocks.csv.zip')
stocks$date <- as.Date(stocks$date)
stocks <- tsibble(stocks, index = date, key = symbol)

#create new df showing stocks on the min date and max date only
last_day <- stocks %>% 
  filter(stocks$date == max(stocks$date))
first_day <- stocks %>% 
  filter(stocks$date == min(stocks$date))
#new df with new column growth(max date closing price 
#minus min date closing price)
newdf <- merge(last_day,first_day, by = "symbol")
newdf$growth <- newdf$close.x - newdf$close.y

#ranking stocks from highest difference to lowest
newdf <- newdf[order(newdf$growth),]
newdf2 <- newdf[order(nrow(newdf):1),]
#deleting columns not needed for leaderboard 
newdf2$gics_sector <- newdf2$gics_sector.x
newdf2 <- newdf2 %>% 
  select(-ends_with('.y')) %>% 
  select(-ends_with('.x'))


ui <- fluidPage(
  pickerInput(
    inputId = "stock",
    label = "Pick one or more stocks.",
    selected = "AAPL",
    choices = unique(stocks$symbol),
    options = list(
      `live-search` = TRUE
    ), multiple = TRUE
  ),
  radioButtons(
    inputId = "selected_col",
    label = "What would you like to plot?",
    choices = c(
      "open", "close",
      "high", "low", "volume"
    )
  ),
  dateRangeInput(
    inputId = "date_range",
    label = "Choose Your Date Range",
    start = min(stocks$date),
    end = max(stocks$date),
    min = min(stocks$date),
    max = max(stocks$date)
  ),
  actionBttn("action", label = "Analyze Lag!", color = "primary"),
  plotOutput("plot"),
  plotOutput("plot2"),
  plotOutput("plot3"),
  pickerInput(
    inputId = "selected_sector",
    label = "Choose Industry and See Stocks With the Largest Growth",
    choices = unique(newdf2$gics_sector)
  ),
  tableOutput("table"),
  
  numericInput("investment", label = h4("What if I had invested ___ dollars?"), 
               value = 1),
  
  textInput("text", label = h4("Input desired stock symbol"), 
  value = "AAPL"),hr(),fluidRow(column(3, verbatimTextOutput("value"))),
  
  plotOutput("forecast")
  
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    stocks[, c("symbol", "date", input$selected_col)] %>%
      filter(symbol == input$stock) %>%
      filter(date %in% seq.Date(from = input$date_range[1], to = input$date_range[2], by = "day")) %>%
      autoplot() +
      labs(title = input$stock)
  })
  observeEvent(input$action,
  output$plot2 <- renderPlot({
    stocks[, c("symbol", "date", input$selected_col)] %>%
      filter(symbol == input$stock) %>%
      gg_lag(geom = 'point') +
      labs(title = input$stock, subtitle = "Lag Plot!")
  }))
  output$table <- renderTable({
    sectorFilter <- head(subset(newdf2, newdf2$gics_sector == input$selected_sector), n = 10)
  })
  
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

      print(input$investment + earnings)})
   

}
shinyApp(ui, server)
