library(shiny)
library(fpp3)
library(readr)

stocks <- read_csv('nyse_stocks.csv.zip')


ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)