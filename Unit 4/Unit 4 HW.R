install.packages("ggthemes")
library(ggthemes)
############ Part 1 ####################

#Live Session 4 For Live Session Web Scraping Code

library(XML) # For XML parsing
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) # use for non standard XML
library(ggplot2)
library(RCurl) #getURL

#Breakout 1 Restaurants!  

#Using rvest

restaurants<-read_html("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")
restaurants_name <- html_nodes(restaurants,"name") # Parse names of the restaurant
restaurants_zip <- html_nodes(restaurants,"zipcode") # Parse zipcode of the restaurant
restaurants_district <- html_nodes(restaurants,"councildistrict") # Parse city council district for the restaurant

restaurants_name
restaurants_zip 
restaurants_district

#### DO NOT RUN MORE THAN ONCE ##################
restaurants_name = stri_sub(restaurants_name,7,-8)
restaurants_zip = stri_sub(restaurants_zip,10,-11)
restaurants_district <- stri_sub(restaurants_district,18,-19)

str(restaurants_district)
restaurants_zip = as.numeric(restaurants_zip)
restaurants_district = as.numeric(restaurants_district)



#How many restaurants total 
#restByDist = hist(hp_councildistrict2)
#barplot(height = restByDist$counts, names = (as.character(seq(1,13,1))),xlab = "Council District",ylab = "Number of Restaurants")
#barplot(height = restByDist$counts, names = (as.character(seq(1,13,1))),xlab = "Number of Restaurants",ylab = "Council District", horiz = TRUE)

restaurants_DF = data.frame(Name = restaurants_name, ZipCode = restaurants_zip, District = restaurants_district)

#How many Sushi Restaurants?
restaurants_DT = restaurants_DF %>% filter(District == "11")
sushi <- grep("sushi",restaurants_DF$Name, ignore.case = TRUE)
sushiDT<- grep("sushi",restaurants_DT$Name, ignore.case = TRUE)
sushicount = count(sushi)
grepl("sushi",restaurants_DF$Name,ignore.case = T)


######## BAR Plot ########

restaurants_DF$District = as.factor(restaurants_DF$District)
restaurants_DF %>% ggplot(aes(x = District, fill = District)) + geom_bar(stat = "count") + ggtitle("Number of Restaurants in Baltimore by District")+coord_flip()

#######################################################################################
############################### Part 2 ################################################
#######################################################################################

######################## CODE 1 - INTERINIO ###########################################
# Library used
library(httr)
library(jsonlite)
baseURL <- "https://api-v2.intrinio.com/securities/"
endpoint <- "prices"
stock <- "MSFT"
APIkey = "OjQ4Zjg2YjgwMGMwYzBlM2M1YjU0NDc3NTA1OTZkODM2"

# Creating the URL
fullURL <- paste(baseURL,stock,"/", endpoint,"?", 'api_key=', APIkey,sep='')
fullURL
# Getting the Data in JSON
msft_json <- jsonlite::fromJSON(fullURL, flatten=TRUE)
msft_json

# Creating the Data Frame
msft_df <- as.data.frame(msft_json)
msft_df
msft_df$stock_prices.date = as.Date(msft_df$stock_prices.date)
par(mfrow = c(2,1))
ggplot(data = msft_df, aes(x = stock_prices.date, y = stock_prices.close))+ geom_line() + ggtitle("Daily Close Price")
msft_df %>% ggplot(aes(x = stock_prices.date, y = stock_prices.volume))+ geom_line() + ggtitle("Daily Volume")


## Plot - Closing and Opening price of Microsoft. Two seperate variables.

msft_df %>% ggplot(aes(x=stock_prices.date)) + 
  geom_line(aes(y = stock_prices.close, color = "Price at Close")) + 
  geom_line(aes(y = stock_prices.open, color="Price at Open"),linetype="twodash") + ggtitle("Daily Open and Close Price of Microsoft") + xlab("Month") + ylab("Stock Prices")+theme_fivethirtyeight()
        


######################## CODE 2 - YAHOO ###############################################


# Install a new Package "Quantmod"
install.packages("quantmod")
library(quantmod)

#Get Microsoft Prices
getSymbols("MSFT", src = "yahoo", from = "2015-01-01", to = Sys.Date(), auto.assign = TRUE)
head(MSFT)
tail(MSFT)
str(MSFT)

# Get APPL prices
getSymbols("AAPL", src = "yahoo", from = "2015-01-01", to = Sys.Date(), auto.assign = TRUE)

ggplot(MSFT, aes(x = index(MSFT), y = MSFT.Close)) + geom_line(color = "darkblue") + ggtitle("Microsoft prices series") + xlab("Date") + ylab("Price") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

#Taking subset of the Data

MSFT_mm <- subset(MSFT, index(MSFT) >= "2019-01-01")

#Calculating Moving Average

MSFT_mm10 <- rollmean(MSFT_mm$MSFT.Close, 10) # 10 Day Moving Average
MSFT_mm30 <- rollmean(MSFT_mm$MSFT.Close, 30) # 30 Day Moving Average


#Adding to the Dataset

MSFT_mm$mm10 <- MSFT_mm10
MSFT_mm$mm30 <- MSFT_mm30

#Plotting the moving average

ggplot(MSFT_mm, aes(x = index(MSFT_mm))) +
  geom_line(aes(y = MSFT_mm$MSFT.Close, color = "MSFT")) + ggtitle("Microsoft prices series") +
  geom_line(aes(y = MSFT_mm$mm10, color = "MM10")) +
  geom_line(aes(y = MSFT_mm$mm30, color = "MM30")) + xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") 


combined = data.frame(Date = index(MSFT), Microsoft.Closing = MSFT$MSFT.Close, Apple.Closing = AAPL$AAPL.Close)  ## Creating new Data Frame by combining Apple and Microsoft Data data
comb_scripts = gather(combined,key = "stock", "price", -Date ) ## Creating Data Frame by reducing the data

# Comparing the Closing prices of Apple and Microsoft
comb_scripts %>% ggplot(aes(x=Date, y=price, color = stock)) +   geom_line()+ylab("Closing Price")+scale_x_date(date_labels = "%b %y", date_breaks = "6 months") +
xlab("Date")+ggtitle("Comparing Closing Prices of Apple And Microsoft")


# MSFT_ret <- diff(log(MSFT[,6]))
# MSFT_ret <- MSFT_ret[-1,]
# 
# Op(MSFT)
# Cl(MSFT)
# Ad(MSFT)

# dailyReturn(MSFT)
# weeklyReturn(MSFT)
# monthlyReturn(MSFT)
# quarterlyReturn(MSFT)
# yearlyReturn(MSFT)
# 
# ggplot(MSFT_ret, aes(x = index(MSFT_ret), y = MSFT_ret)) +
#   geom_line(color = "deepskyblue4") +
#   ggtitle("Microsoft returns series") +
#   xlab("Date") + ylab("Return") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")
# 
# 
# MSFT_ret17 <- subset(MSFT_ret, index(MSFT_ret) > "2017-01-01")
# 
# ggplot(MSFT_ret17, aes(x = index(MSFT_ret17), y = MSFT_ret17)) +
#   geom_line(color = "deepskyblue4") +
#   ggtitle("Microsoft returns series From 2017") + xlab("Date") + ylab("Return") +
#   theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

install.packages("tidyquant")
install.packages("tidyverse")
library(tidyquant) 
library(tidyverse)
# 
######### ERROR ################################
######### Candlestick not working ###############

# MSFT %>%
#   ggplot(aes(x = index(MSFT), y = MSFT.Close, open = MSFT.Open,
#              high = MSFT.High, low = MSFT.Low, close = MSFT.Close)) +
#   geom_candlestick() +
#   geom_bbands(ma_fun = SMA, sd = 2, n = 20) +
#   labs(title = "Microsoft Candlestick Chart",
#        subtitle = "BBands with SMA Applied",
#        y = "Closing Price", x = "") 





# 
# ####
# MSFT %>%
#   ggplot(aes(x = index(MSFT), y = MSFT.Close)) +
#   geom_boxplot(aes(ymax = MSFT.High, ymin = MSFT.Low),color = "darkgreen", colour_down = "darkred", size = 1) +
#   labs(title = "Microsoft Candlestick Chart", 
#        subtitle = "BBands with SMA Applied", 
#        y = "Closing Price", x = "") + 
#   coord_x_date(xlim = c(end - weeks(24), end),
#                ylim = c(100, 120)) + 
#   theme_tq()

comb_scripts %>% ggplot(aes(x=Date, y=price)) + 
  geom_line(aes(y = MSFT.Close), color = "darkred") + 
  geom_line(aes(y = AAPL.Close), color="steelblue", linetype="twodash") + ggtitle("Daily Open and Close Price of Microsoft") + xlab("Month") + ylab("Stock Prices")+
  scale_colour_manual("Legend", values=c("Price Open"="steelblue", "Price Close"="darkred"))

