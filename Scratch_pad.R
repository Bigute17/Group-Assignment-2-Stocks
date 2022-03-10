library(fpp3)
library(readr)

# Read zipped data
stocks <- read_csv("nyse_stocks.csv.zip")

# Convert to `tsibble()`
stocks$date <- as.Date(stocks$date)
stocks <- tsibble(stocks, index = date, key = symbol)

# 1 stock
selected_stock <- "AAPL"

stocks %>%
  filter(symbol == selected_stock) %>%
  autoplot(open) +
  labs(title = selected_stock)

# Multiple stocks
selected_stocks <- c("GOOG", "AAPL")

stocks %>%
  filter(symbol %in% selected_stocks) %>%
  autoplot(open)

#Filtering by sub industry and plotting
stocks %>% 
  filter(gics_sub_industry == "Soft Drinks") %>% 
  autoplot(open)

for (i in 1:length(unique(stocks$symbol))) {
  stocks$performance[i] <- stocks$close[stocks$symbol == stocks$symbol[i] & stocks$date == "2016-12-30"] 
  - stocks$open[stocks$symbol == stocks$symbol[i] & stocks$date == "2010-01-04"] }
