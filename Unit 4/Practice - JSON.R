library(httr)
library(jsonlite)
base <- "https://api-v2.intrinio.com/securities/"
endpoint <- "prices"
stock <- "MSFT"
mykey = "OjQ4Zjg2YjgwMGMwYzBlM2M1YjU0NDc3NTA1OTZkODM2"
call1 <- paste(base,stock,"/", endpoint,"?", 'api_key=', mykey,sep='')
call1
msft_json <- jsonlite::fromJSON(call1, flatten=TRUE)
msft_json
msft_df <- as.data.frame(msft_json)
msft_df
msft_df$stock_prices.date = as.Date(msft_df$stock_prices.date)
par(mfrow = c(2,1))
msft_df %>% ggplot(aes(x = stock_prices.date, y = stock_prices.close))+ geom_line() + ggtitle("Adjusted Close")
msft_df %>% ggplot(aes(x = stock_prices.date, y = stock_prices.volume))+ geom_col() + ggtitle("Adjusted Volume")

###########################################3
library(httr)
library(jsonlite)

username = "cc092ea39fffe74b69f12976f4bfcd22"
password = "7a8adcf931edb0766ff36fbb0b5cc421"

base <- "https://api.intrinio.com/"
endpoint <- "prices"
stock <- "AAPL"

call1 <- paste(base,endpoint,"?","ticker","=", stock, sep="")

call1

get_prices <- GET(call1, authenticate(username,password, type = "basic"))

get_prices_text <- content(get_prices, "text")

get_prices_json <- jsonlite::fromJSON(get_prices_text, flatten = TRUE)

get_prices_df <- as.data.frame(get_prices_json$data)

get_prices_df$date = as.Date(get_prices_df$date)

par(mfrow = c(2,1))

get_prices_df %>% ggplot(aes(x = date, y = adj_close))+ geom_line() + ggtitle("Adjusted Close")
get_prices_df %>% ggplot(aes(x = date, y = adj_volume))+ geom_col() + ggtitle("Adjusted Volume")

