library(shiny)
library(fpp3)
library(readr)
library(shinyWidgets)

stocks <- read_csv("nyse_stocks.csv.zip")
stocks$date <- as.Date(stocks$date)
stocks <- tsibble(stocks, index = date, key = symbol)

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
  plotOutput("plot"),
  pickerInput(
    inputId = "selected_sector",
    label = "Choose A Sector",
    choices = unique(newdf2$gics_sector)
  ),
  tableOutput("table")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    stocks[, c("symbol", "date", input$selected_col)] %>%
      filter(symbol == input$stock) %>%
      filter(date %in% seq.Date(from = input$date_range[1], to = input$date_range[2], by = "day")) %>%
      autoplot() +
      labs(title = input$stock)
  })
  output$table <- renderTable({
    sectorFilter <- head(subset(newdf2, newdf2$gics_sector == input$selected_sector), n = 10)
  })
}

shinyApp(ui, server)
