library(quantmod)
library(readr)
library(dplyr)
library(TTR)
library(caret)

library(caretEnsemble)

#Downloading data from yahoo

getSymbols('GOOG', from = '2013-06-01', src = "yahoo")
getSymbols('^GSPC', from = '2013-06-01', src = "yahoo")
stock_data <- data.frame(date=index(GOOG), coredata(GOOG))
s_p <- data.frame(date=index(GSPC), coredata(GSPC))
date_df <- as.data.frame(stock_data[,1])

google_trends <- read_csv("C:/Users/Varun/Desktop/ASU/512/Project/google_trends.csv", 
                          col_types = cols(date = col_date(format = "%m/%d/%Y")))

#List of stocks
tickers <- c("NFLX", "GOOG","AAPL","IDN","INVA","KNDI","FCAU","TOWR","SORL","TSLA","DVN","ANW","MRO","SM","BP","RELV","PEP","SBUX","CXDC","BAC","QIWI","GNW","AXP")
test <- list()
to_do <- as.data.frame(tickers)
to_do$Signal <- ""
to_do$Signal2 <- ""
for (i in 1:length(tickers)) {
  test[[i]] <- getSymbols(tickers[i], from = '2013-06-01',src="yahoo", auto.assign = FALSE)
}
test_model <- list()

#S&P500 Rate of Return
s_p$sp500_ror <- 0
for(i in 2:nrow(s_p)){
  s_p[i,8] <- (s_p[i,7]-s_p[i-1,7])/s_p[i-1,7]
}
s_p <- s_p[,c(1,8)]

#For each stock
for(i in 1:length(tickers)){
temp <- test[i]
stock_data <- data.frame(date=index(temp), coredata(temp))
stock_data$date <- date_df$`stock_data[, 1]` 
rownames(stock_data) <- c(1:nrow(stock_data))
stock_data$movement <- "HOLD"
stock_data$ROR <- 0
stock_data$MA20 <- SMA(stock_data[,7],n=5)
stock_data$rorxvol <- 0
for(j in 6:nrow(stock_data)){
  if(j <nrow(stock_data)){
  x <-(stock_data[j,7]-stock_data[j-1,10])/stock_data[j-1,10] 
  if(x >= 0.01){
    stock_data[j-1,8] <- "BUY"
  }
  if(x <= -0.01){
    stock_data[j-1,8] <- "SELL"
  }
  }
  }
for(j in 2:nrow(stock_data)){
  if(j > 1){
  stock_data[j,9] <- (stock_data[j,7]-stock_data[j-1,7])/stock_data[j-1,7]
  stock_data[j,11] <- (stock_data[j,9]*stock_data[j,6])
  }
}


#Generating signals from Bollinger Band
bb <- BBands(cbind(Hi(stock_data),Lo(stock_data),Cl(stock_data)),n=20,sd=2)

bbtr <- Lag(ifelse(Lag(Cl(stock_data))<Lag(bb[,1])&Cl(stock_data)>bb[,1],1,ifelse(Lag(Cl(stock_data))<Lag(bb[,3])&Cl(stock_data)>bb[,3],-1,0)))
bbtr[is.na(bbtr)] <- 0
bb_stockdata <- as.data.frame(bbtr)
bb_stockdata$date <- stock_data$date

#Generating investment decisions from MACD
macd <- MACD(Cl(stock_data),nFast=12,nSlow=26,nSig=9)
smacdtr <- Lag(ifelse(Lag(macd[,1])<Lag(macd[,2])&macd[,1]>macd[,2],1,ifelse(Lag(macd[,1])>Lag(macd[,2])&macd[,1]<macd[,2],-1,0)))
smacdtr[is.na(smacdtr)] <- 0
macd_stockdata <- as.data.frame(smacdtr)
macd_stockdata$date <- stock_data$date

#Generating investment decisions from RSI
rsi <- RSI(Cl(stock_data),n=14)
rsitr <- Lag(ifelse(Lag(rsi)<30&rsi>30,1,ifelse(Lag(rsi)<70&rsi>70,-1,0)))
rsitr[is.na(rsitr)] <- 0
rsi_stockdata <- as.data.frame(rsitr)
rsi_stockdata$date <- stock_data$date

#Generating investment decisions from ADX
adx <- ADX(cbind(Hi(stock_data),Lo(stock_data),Cl(stock_data)),n=14)
adxtr <- Lag(ifelse(Lag(adx[,1])<Lag(adx[,2])&adx[,1]>adx[,2]&adx[,4]>20,1,ifelse(Lag(adx[,1])>Lag(adx[,2])&adx[,1]<adx[,2]&adx[,4]>20,-1,0)))
adxtr[is.na(adxtr)] <- 0
adx_stockdata <- as.data.frame(adxtr)
adx_stockdata$date <- stock_data$date

#adding technical indiactors to the stock data
stock_data <- stock_data %>%
  inner_join(bb_stockdata,by = 'date')

stock_data <- stock_data %>%
  inner_join(macd_stockdata,by = 'date')

stock_data <- stock_data %>%
  inner_join(rsi_stockdata,by = 'date')

stock_data <- stock_data %>%
  inner_join(adx_stockdata,by = 'date')

stock_data <- stock_data %>%
  inner_join(s_p,by = 'date')

stock_data <- stock_data %>%
  inner_join(google_trends,by = 'date')

#Preparing data for model building
data_for_model <- stock_data[-c(1:5),c(8,11:21)]
colnames(data_for_model) <- c("Direction","rorxvol","BB","MACDS","RSI","ADX","Market_ROR","1","2","3","4","5")
factor_col <- c("Direction","BB","MACDS","RSI","ADX")
data_for_model[,factor_col] <- lapply(data_for_model[,factor_col] , factor)
to_test <- data_for_model[nrow(data_for_model),-1]


#Training Set
trainingindex <- createDataPartition(data_for_model$Direction,p=0.75, list=FALSE,times=1)
train_data <- data_for_model

# define training control
train_control <- trainControl(method="cv", number=10)

#adaptive neural network filter
print(tickers[i])
filter <- caret::train(Direction ~ ., data = train_data, method="nnet", trControl=train_control)
to_do[i,2] <- (predict(model,to_test))
}

write.csv(to_do,'C:/Users/Varun/Desktop/Results.csv')
