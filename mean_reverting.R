library(forecast) 
library(tseries) 
library(urca) 
library(xts) 
library(dplyr)
library(vars)
library(PerformanceAnalytics)
# load
data = "fx"
sep = "/"
from = "2008-01"
to = "2014-06"
max_port = 4
portfolio <- list()
for (ccy in  list.files("fx")) {
  print (paste("loading ", ccy))
  dat = read.csv(paste(data,ccy, sep=sep), header=T) 
  date = as.Date(dat[,1], format = "%Y.%m.%d %H:%M:%S") 
  ccy <- gsub(".csv", "", ccy)
  if(startsWith(ccy, "USD")) {
    ccy <- gsub("USD", "", ccy)
    ccy <-paste0(ccy,"USD")
    pair = data.frame(Bid=1/dat[,"Open"], Volume=dat[,"Volume"])
  } else {
    pair = data.frame(Bid=dat[,"Open"], Volume=dat[,"Volume"])
  }
  colnames(pair) <- c(ccy,"Volume")
  pair = as.xts(pair, date) 
  pair <- pair[paste(from,to,sep=sep)]
  portfolio[[ccy]] <- pair 
}
# create test portfolios
cointegrationTestPortfolioCollection = list()
for(i in seq(2,max_port)) {
  combinations = t(combn(length(portfolio),i))
  for(j in seq(1,dim(combinations)[1])) {
    # create a Portfolio containing z CurrencyPairs for which we want to perform the test
    toTest = vector("list",dim(combinations)[2])
    for(z in seq(1:dim(combinations)[2])) {
      # Add the currency pairs that correspond to this particular combination to the list
      toTest[[z]] = portfolio[[combinations[j,z]]]
    }
    prices <- data.frame(toTest[[1]][,1])
    colnames(prices) <- colnames(toTest[[1]])[1]
    for(i in 2:length(toTest)) {
      nxt <- data.frame(toTest[[i]][,1])
      colnames(nxt) <- colnames(toTest[[i]])[1]
      prices <- cbind(prices,nxt)
    }
    varest <- VAR(prices,p=1,type="const",lag.max=24, ic="SC")
    # in the Johansen procedure for cointegration a lagged VAR (VECM) is used. Hence we need to subtract 1 from the optimal VAR lag length.
    lagLength <- max(2,varest$p-1)
    print(paste("Testing", colnames(prices)))
    res <- ca.jo(prices,type="trace",ecdet="const",K=lagLength,spec="longrun")
  }
}