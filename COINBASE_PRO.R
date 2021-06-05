install.packages("devtools");require("devtools")
install_github("DheerajAgarwal/rgdax", ref="dev")
require("rgdax");require("httr");require("dplyr")
require("lubridate");require("quantmod");require("pbapply")
# https://cran.r-project.org/web/packages/rgdax/rgdax.pdf
#*************************************************************************************
#                          # API KEYS / ACCOUNT ID
#*************************************************************************************
KEYS <- new.env()
assign("apikey","******************",envir = KEYS)
assign("PASS","******************",envir = KEYS)
assign("apiSecret","******************",
       envir = KEYS)
#*************************************************************************************
#*************************************************************************************
# will list all account IDs for different cryptos
allAccts <- rgdax::accounts(api.key = KEYS$apikey,secret = KEYS$apiSecret, passphrase = KEYS$PASS)
# allAccts <- allAccts[,-1]
# allAccts <- allAccts[,-5]
# assign Acct ID for 'BTC'
assign("acctID",allAccts[allAccts$currency == "ETH",]$id,env=KEYS)
# account specific information
ACCT <- rgdax::account(acct_id=KEYS$acctID,api.key = KEYS$apikey,
                       secret = KEYS$apiSecret, passphrase = KEYS$PASS)
# ACCT[1,2] <- NA
# ACCT[6,2] <- NA
# account information
rgdax::holds(currency = "ETH", api.key=KEYS$apikey, secret=KEYS$apiSecret, passphrase=KEYS$PASS)
# ******************************************************************************************
#                                   24-hour stats for a given currency
# ******************************************************************************************
daily_Stats <- rgdax::public_daystats(product_id = "ETH-USD")
# ******************************************************************************************
#                                       public order book
# ******************************************************************************************
OrderBk1 <- rgdax::public_orderbook(product_id = "ETH-USD", level = 1)
OrderBk2 <- rgdax::public_orderbook(product_id = "ETH-USD", level = 2)
OrderBk3 <- rgdax::public_orderbook(product_id = "ETH-USD", level = 3)
# ******************************************************************************************
#                                       Get BEST bid/ask
# ******************************************************************************************
# https://docs.pro.coinbase.com/?python#get-product-order-book
get_best_bidAsk = function(KEYS,COIN)
{
  # BUILD COINBASE PRO URL
  url <- paste0("https://api.pro.coinbase.com/products/",COIN,"-USD/book")
  
  # GET REQUEST
  exp <- GET(url,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Key" = KEYS$apikey,
                         "Passphrase"= KEYS$PASS,
                         "Secret" = KEYS$apiSecret))
  # format request
  dta <- fromJSON(url,simplifyVector = TRUE)
  
  # get bid/ask
  bid = dta$bids %>% as.data.frame
  colnames(bid) <- c("bid.Price","bid.Size","bid.numOrders")
  bid <- bid[,c("bid.numOrders","bid.Size","bid.Price")]
  ask = dta$asks %>% as.data.frame
  colnames(ask) <- c("ask.Price","ask.Size","ask.numOrders")
  bidask = cbind(bid,ask)
  # change to numeric columns
  bidask$bid.numOrders <- as.numeric(bidask$bid.numOrders)
  bidask$bid.Size <- as.numeric(bidask$bid.Size)
  bidask$bid.Price <- as.numeric(bidask$bid.Price)
  bidask$ask.numOrders <- as.numeric(bidask$ask.numOrders)
  bidask$ask.Size <- as.numeric(bidask$ask.Size)
  bidask$ask.Price <- as.numeric(bidask$ask.Price)
  # Spread
  bidask$Spread = bidask$ask.Price - bidask$bid.Price
  # return data
  bidask
}
bAsk = get_best_bidAsk(KEYS,COIN="ETH")
bAsk = get_best_bidAsk(KEYS,COIN="XTZ")
# ******************************************************************************************
#                                       Get Level 2 Order Book
# ******************************************************************************************
# https://docs.pro.coinbase.com/?python#get-product-order-book
get_level2_orderbook = function(KEYS,COIN)
{
  # BUILD COINBASE PRO URL
  url <- paste0("https://api.pro.coinbase.com/products/",COIN,"-USD/book?level=2")
  
  # GET REQUEST
  exp <- GET(url,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Key" = KEYS$apikey,
                         "Passphrase"= KEYS$PASS,
                         "Secret" = KEYS$apiSecret))
  # format request
  dta <- fromJSON(url,simplifyVector = TRUE)
  
  # get bid/ask
  bid = dta$bids %>% as.data.frame
  colnames(bid) <- c("bid.Price","bid.Size","bid.numOrders")
  bid <- bid[,c("bid.numOrders","bid.Size","bid.Price")]
  ask = dta$asks %>% as.data.frame
  colnames(ask) <- c("ask.Price","ask.Size","ask.numOrders")
  bidask = cbind(bid,ask)
  # numeric columns
  bidask$bid.numOrders <- bidask$bid.numOrders %>% as.numeric
  bidask$bid.Size      <- bidask$bid.Size %>% as.numeric
  bidask$bid.Price     <- bidask$bid.Price %>% as.numeric
  bidask$ask.Price     <- bidask$ask.Price %>% as.numeric
  bidask$ask.Size      <- bidask$ask.Size %>% as.numeric
  bidask$ask.numOrders <- bidask$ask.numOrders %>% as.numeric
  # return data
  bidask
}
l2 = get_level2_orderbook(KEYS,COIN="ETH")
l2 = get_level2_orderbook(KEYS,COIN="BTC")
plot(l2$bid.Price,l2$bid.Size, type = 'h',xlim = c(min(l2$bid.Price),max(l2$ask.Price)), col="green")
lines(l2$ask.Price,l2$ask.Size,type="h",col="red")
abline(v=(l2$bid.Price[1]+l2$ask.Price[1])/2)
# ******************************************************************************************
#                                       Public Trades
# ****************************************************************************************** 
# get latest trades
crypto_BTC_trades<- rgdax::public_trades(product_id = "BTC-USD")
crypto_ETH_trades<- rgdax::public_trades(product_id = "ETH-USD")
# ******************************************************************************************
#                                       add Orders
# ****************************************************************************************** 
# type : Optional character value for the order type. The default is "limit"
# stop : Possible values apart from default NULL are "loss" or "entry". 
# side : The default is "b" which stands for buy must be one of either "b" (buy) or "s" (sell).
# price: It can either be an integer or float. Float values rounded to 2 decimals
# size : Mandatory numeric value. It can either be an integer or float. Float values will NOT be rounded
rgdax::add_order(api.key=KEYS$apikey, secret=KEYS$apiSecret, passphrase=KEYS$PASS, 
                 product_id = "ETH-USD",type = "limit", stop = NULL, 
                 stop_price = NULL, side = "s",price = 4000, size=0.0001088)
# Minimum size is 0.00100000 ETH
# View SIZE limits
sizeLMT <- public_info(product=TRUE)

# cancel all orders
rgdax::cancel_order(order_id = "all", api.key=KEYS$apikey, secret=KEYS$apiSecret, passphrase=KEYS$PASS)
# get fills 
rgdax::fills(api.key=KEYS$apikey, secret=KEYS$apiSecret, passphrase=KEYS$PASS, product_id = NULL)

# ******************************************************************************************
#                                       get historical data
# ****************************************************************************************** 
# https://docs.pro.coinbase.com/?python#get-historic-rates
# start times must be include milliseconds ex:  2021-05-28T23:47:25.201Z

# granularity: limited to 300 bars
# 60   :  1 minute bars
# 300  :  5 minute bars 
# 900  : 15 minute bars 
# 3600 :  1   hour bars 
# 21600:  6   hour bars 
# 86400:  1    day bars 

# calculates START time in order to get max number of bars (300) 
# for each interval/granularity
get_START = function(NOW, Interval)
{
  if(Interval == 60)
  {
    START = as.character(strftime(NOW - minutes(300), "%Y-%m-%dT%H:%M:%S.500Z"))
  }
  if(Interval == 300)
  {
    START = as.character(strftime(NOW - minutes(1500), "%Y-%m-%dT%H:%M:%S.500Z"))
  }
  if(Interval == 900)
  {
    START = as.character(strftime(NOW - minutes(4500), "%Y-%m-%dT%H:%M:%S.500Z"))
  }
  if(Interval == 3600)
  {
    START = as.character(strftime(NOW - hours(300), "%Y-%m-%dT%H:%M:%S.500Z"))
  }
  if(Interval == 21600)
  {
    START = as.character(strftime(NOW - hours(1800), "%Y-%m-%dT%H:%M:%S.500Z"))
  }
  if(Interval == 86400)
  {
    START = as.character(strftime(NOW - days(300), "%Y-%m-%dT%H:%M:%S.500Z"))
  }
  
  return(START)
}
# wrapper to get most recent bars
get_latest_crypto_bars = function(KEYS,COIN, Interval)
{
  # Current time
  NOW = Sys.time()
  # Start Time
  START = get_START(NOW=NOW,Interval=Interval)
  # End Time
  END   = as.character(strftime(NOW , "%Y-%m-%dT%H:%M:%S.500Z"))
  
  # BUILD COINBASE PRO URL
  url <- paste0("https://api.pro.coinbase.com/products/",COIN,"/candles?start=",START,
                "&end=",END,"&granularity=",Interval)
  # GET REQUEST
  exp <- GET(url,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Key" = KEYS$apikey,
                         "Passphrase"= KEYS$PASS,
                         "Secret" = KEYS$apiSecret))
  # format request
  dta <- fromJSON(url,simplifyVector = TRUE)
  # rbind bars
  #dta <- do.call(rbind,dta) %>% as.data.frame()
  dta <- dta %>% as.data.frame()
  # format column names
  colnames(dta)<- c("Time","Low","High","Open","Close","Volume")
  # local timezone adjustment
  tmDIFF = round(as.numeric(difftime(Sys.time(),
                                     lubridate::force_tz(with_tz(Sys.time(),tz="UTC")),
                                     units = "hours")),0)
  if(Interval == 86400)
  {
    # format timeStamps
    Time <- as.Date(as.POSIXct(dta$Time, origin = "1970-01-01",tz = Sys.timezone()) - hours(tmDIFF))
  }else{
    # format timeStamps
    Time <- as.POSIXct(dta$Time, origin = "1970-01-01",tz = Sys.timezone()) - hours(tmDIFF)
  }
  
  # convert to xts
  dta <- as.xts(dta[,c("Open","High","Low","Close","Volume")], order.by = Time)
  # return data
  dta
}

btcBars <- get_latest_crypto_bars(KEYS,COIN="BTC-USD",Interval = 60)
btcBars <- get_latest_crypto_bars(KEYS,COIN="BTC-USD",Interval = 300)
btcBars <- get_latest_crypto_bars(KEYS,COIN="BTC-USD",Interval = 86400)
# ******************************************************************************************
#               getting historical bars - gets max bars up to END date
# ******************************************************************************************
# wrapper to get historical data up to the END_DATE & END_TIME
# format example:
# END_DATE = "2021-05-29"
# END_TIME = "14:00:00"
get_historical_crypto_bars = function(KEYS,COIN,Interval,END_DATE, END_TIME)
{
  # Will set it to Midnight so we get the most data ;)
  NOW = as.POSIXct(paste0(END_DATE, " ", END_TIME), format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
  # End Time
  END   = as.character(strftime(NOW , "%Y-%m-%dT%H:%M:%S.500Z"))
  # Start Time
  START = get_START(NOW=NOW,Interval=Interval)
  
  # BUILD COINBASE PRO URL
  url <- paste0("https://api.pro.coinbase.com/products/",COIN,"-USD/candles?start=",START,
                "&end=",END,"&granularity=",Interval)
  # GET REQUEST
  exp <- GET(url,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Key" = KEYS$apikey,
                         "Passphrase"= KEYS$PASS,
                         "Secret" = KEYS$apiSecret))
  # format request
  dta <- fromJSON(url,simplifyVector = TRUE)
  # rbind bars
  #dta <- do.call(rbind,dta) %>% as.data.frame()
  dta <- dta %>% as.data.frame()
  # format column names
  colnames(dta)<- c("Time","Low","High","Open","Close","Volume")
  # local timezone adjustment
  tmDIFF = round(as.numeric(difftime(Sys.time(),
                                     lubridate::force_tz(with_tz(Sys.time(),tz="UTC")),
                                     units = "hours")),0)
  if(Interval == 86400)
  {
    # format timeStamps
    Time <- as.Date(as.POSIXct(dta$Time, origin = "1970-01-01",tz = Sys.timezone()) - hours(tmDIFF))
  }else{
    # format timeStamps
    Time <- as.POSIXct(dta$Time, origin = "1970-01-01",tz = Sys.timezone()) - hours(tmDIFF)
  }
  
  # convert to xts
  dta <- as.xts(dta[,c("Open","High","Low","Close","Volume")], order.by = Time)
  # return data
  dta
}

# create a sequence of times to get/call data
# tSeq = seq.POSIXt(from = as.POSIXct("2019-10-01 00:00:00"), to = Sys.time(), by = "1 min");INTERVAL<- 60
# tSeq = seq.POSIXt(from = as.POSIXct("2019-10-01 00:00:00"), to = Sys.time(), by = "5 min");INTERVAL<- 300
# tSeq = seq.POSIXt(from = as.POSIXct("2019-10-01 00:00:00"), to = Sys.time(), by = "15 min");INTERVAL<- 900
# tSeq = seq.POSIXt(from = as.POSIXct("2019-10-01 00:00:00"), to = Sys.time(), by = "1 hour");INTERVAL<- 3600
# tSeq = seq.POSIXt(from = as.POSIXct("2019-10-01 00:00:00"), to = Sys.time(), by = "6 hour");INTERVAL<- 21600
# tSeq = seq.POSIXt(from = as.POSIXct("2019-10-01 00:00:00"), to = Sys.time(), by = "1 day");INTERVAL<- 86400

tSeq = seq.POSIXt(from = as.POSIXct("2019-10-01 00:00:00"), to = Sys.time(), by = "5 min");INTERVAL<- 300
ii = rev(seq(1,length(tSeq),300))

# extract endpoints
endP = c(tSeq[length(tSeq)],tSeq[ii])
# crypto to call
COIN = "ETH"

ALL = pblapply(as.list(1:length(endP)),FUN=function(ii){
  timeS = endP[ii]
  END_DATE = as.character(strftime(timeS, "%Y-%m-%d"))
  END_TIME = as.character(strftime(timeS, "%H:%M:%S")) 
  df <- try(get_historical_crypto_bars(KEYS=KEYS,COIN=COIN,Interval = INTERVAL, 
                                       END_DATE = END_DATE, END_TIME = END_TIME))
  if(!inherits(df,'try-error')) df
})
# remove empty lists
ALL<-ALL[lapply(ALL,length)>0]
# rowbind data
ALL <- do.call(rbind,ALL)
# drop duplicates 
ALL <- make.index.unique(ALL,drop=TRUE,fromLast = TRUE)
# see lengths of Dates to see if we are missing any
length(unique(as.Date(index(ALL))))
length(seq.Date(as.Date("2019-10-01"),Sys.Date(), 1))

as.Date(setdiff(unique(as.Date(index(ALL)), format="%Y-%m-%d"),
                seq.Date(as.Date("2019-10-01"),Sys.Date(), 1)))

# adjust colnames -> easier to work with using quantmod
colnames(ALL) = paste0(COIN,".",names(ALL)) 
# plot the month of May
chartSeries(ALL["202105"])
