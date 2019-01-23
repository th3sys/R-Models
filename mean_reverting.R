library(forecast) 
library(tseries) 
library(urca) 
library(xts) 
library(dplyr)
library(vars)
library(PerformanceAnalytics)
# 1. load
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
  colnames(pair) <- c(ccy,paste(ccy,"Volume",sep="_"))
  pair = as.xts(pair, date) 
  pair <- pair[paste(from,to,sep=sep)]
  portfolio[[ccy]] <- pair 
}
# 2. create portfolios and test
tests = list()
for(i in seq(2,max_port)) {
  combinations = t(combn(length(portfolio),i))
  for(j in seq(1,dim(combinations)[1])) {
    # create a Portfolio containing z CurrencyPairs for which we want to perform the test
    toTest = vector("list",dim(combinations)[2])
    for(z in seq(1:dim(combinations)[2])) {
      # Add the currency pairs that correspond to this particular combination to the list
      toTest[[z]] = portfolio[[combinations[j,z]]]
    }
    prices <- data.frame(toTest[[1]])
    # colnames(prices) <- colnames(toTest[[1]])
    for(i in 2:length(toTest)) {
      nxt <- data.frame(toTest[[i]])
      # colnames(nxt) <- colnames(toTest[[i]])
      prices <- cbind(prices,nxt)
    }
    selected = list()
    for(x in colnames(prices)){
      if(!grepl("_Volume",x)) {
        print(x)
        selected[[x]] <- x
      }
    }
    prices <- prices %>% 
      filter_all(all_vars(abs(.) > 0))
    prices <- dplyr::select(prices, as.character(selected))
    varest <- VAR(prices,p=1,type="const",lag.max=24, ic="SC")
    # in the Johansen procedure for cointegration a lagged VAR (VECM) is used. Hence we need to subtract 1 from the optimal VAR lag length.
    lagLength <- max(2,varest$p-1)
    print(paste("Testing", colnames(prices)))
    res <- ca.jo(prices,type="trace",ecdet="const",K=lagLength,spec="longrun")
    result <- list()
    result$portfolio <- toTest
    result$trace <- res
    tests[[length(tests)+1]] <- result
  }
}
print(paste(length(tests)," Portfolio's were tested for cointegration",sep=""))
# 3. select cointegrated
cointegratedPortfolio <- list()
for(i in 1:length(tests))
{
  testStatistics <- tests[[i]]$trace@teststat
  criticalValues <- tests[[i]]$trace@cval
  # If the trace statistic for r ≤ 0 is rejected with at least 90% confidence
  # chi^2. If testStatic for r<= 0 is greater than the corresponding criticalValue, then r<=0 is rejected and we have at least one cointegrating vector
  if(testStatistics[length(testStatistics)] >= criticalValues[dim(criticalValues)[1],1]) {
    cointegratedPortfolio[length(cointegratedPortfolio)+1] <- tests[[i]]
    colnames(tests[[i]]$portfolio)[1]
    selected <- list()
    for (x in 1:length(tests[[i]]$portfolio)) {
      selected[x] <- colnames(tests[[i]]$portfolio[[x]])[1]
    }
    print(paste(selected, collapse=', '))
  }
}
print(paste(length(cointegratedPortfolio), " Portfolio's are cointegrated", sep=""))











