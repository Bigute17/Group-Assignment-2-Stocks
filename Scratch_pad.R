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

stocks$performance <- c()
for (i in 1:length(unique(stocks$symbol))) {
  stocks$performance[i] <- stocks$close[stocks$symbol == stocks$symbol[i] & stocks$date == "2016-12-30"] 
  - stocks$open[stocks$symbol == stocks$symbol[i] & stocks$date == "2010-01-04"] }

test <- c(min(stocks$date), max(stocks$date))
test2 <- seq.Date(from = test[1], to = test[2], by = "day")

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



stocks %>%
  filter(symbol == "ADBE") %>%
  gg_lag(geom = 'point',y = open) 







