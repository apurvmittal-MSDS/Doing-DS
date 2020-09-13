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


########################################################################3
client <- IntrinioSDK::ApiClient$new()

# Configure API key authorization: ApiKeyAuth
client$configuration$apiKey <- "OjJlNDNmNDljMmE1OGZjODMzMzU4YTQ0NTI1YWQ3ZmVm"

# Setup API with client
StockExchangeApi <- IntrinioSDK::StockExchangeApi$new(client)

# Required params
identifier <- "NYSE" # Character | A Stock Exchange identifier (MIC or Intrinio ID)

# Optional params
opts <- list(
  page_size = 100, # Integer | The number of results to return
  next_page = NULL # Character | Gets the next page of data from a previous API call
)

response <- StockExchangeApi$get_stock_exchange_securities(identifier, opts)
response
print(response)
print(response$content)
#################################################

install.packages("quantmod")
library(quantmod)
library(ggplot2)
getSymbols("FB", src = "yahoo", from = "2013-01-01", to = Sys.Date(), auto.assign = TRUE)
head(FB)
tail(FB)
str(FB)

ggplot(FB, aes(x = index(FB), y = FB[,6])) + geom_line(color = "darkblue") + ggtitle("FaceBook prices series") + xlab("Date") + ylab("Price") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "2 months")

FB_mm <- subset(FB, index(FB) >= "2018-01-01")

FB_mm10 <- rollmean(FB_mm[,6], 10, fill = list(NA, NULL, NA), align = "right")
FB_mm30 <- rollmean(FB_mm[,6], 30, fill = list(NA, NULL, NA), align = "right")

FB_mm$mm10 <- coredata(FB_mm10)
FB_mm$mm30 <- coredata(FB_mm30)

ggplot(FB_mm, aes(x = index(FB_mm))) +
  geom_line(aes(y = FB_mm[,6], color = "FB")) + ggtitle("FaceBook prices series") +
  geom_line(aes(y = FB_mm$mm10, color = "MM10")) +
  geom_line(aes(y = FB_mm$mm30, color = "MM30")) + xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_colour_manual("Series", values=c("FB"="gray40", "MM10"="firebrick4", "MM30"="darkcyan"))

FB_ret <- diff(log(FB[,6]))
FB_ret <- FB_ret[-1,]

Op(FB)
Cl(FB)
Ad(FB)

dailyReturn(FB)
weeklyReturn(FB)
monthlyReturn(FB)
quarterlyReturn(FB)
yearlyReturn(FB)

ggplot(FB_ret, aes(x = index(FB_ret), y = FB_ret)) +
  geom_line(color = "deepskyblue4") +
  ggtitle("FaceBook returns series") +
  xlab("Date") + ylab("Return") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")
  
  
  FB_ret17 <- subset(FB_ret, index(FB_ret) > "2017-01-01")

ggplot(FB_ret17, aes(x = index(FB_ret17), y = FB_ret17)) +
  geom_line(color = "deepskyblue4") +
  ggtitle("FaceBook returns series in 2017") + xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")
  
  
  