library(forecast) 
library(tseries) 
library(urca) 
library(xts) 
library(dplyr)
library(vars)
library(PerformanceAnalytics)
# Note: Function was found online and it was copied here
vect_lag <- function(v, n=1, forward=FALSE) {
  if (forward)
    c(v[(n+1):length(v)], rep(NA, n))
  else
    c(rep(NA, n), v[1:(length(v) - n)])
}
getPrices = function(theObject) {
  prices <- data.frame(theObject[[1]])
  # colnames(prices) <- colnames(toTest[[1]])
  for(i in 2:length(theObject)) {
    nxt <- data.frame(theObject[[i]])
    # colnames(nxt) <- colnames(toTest[[i]])
    prices <- cbind(prices,nxt)
  }
  return (prices)
}
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
    prices <- getPrices(toTest)
    selected = list()
    for(x in colnames(prices)){
      if(!grepl("_Volume",x)) {
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
    cointegratedPortfolio[[length(cointegratedPortfolio)+1]] <- tests[[i]]
    colnames(tests[[i]]$portfolio)[1]
    selected <- list()
    for (x in 1:length(tests[[i]]$portfolio)) {
      selected[x] <- colnames(tests[[i]]$portfolio[[x]])[1]
    }
    print(paste(selected, collapse=', '))
  }
}
print(paste(length(cointegratedPortfolio), " Portfolio's are cointegrated", sep=""))
# 4. plot
do.call(file.remove, list(list.files("analyse/", full.names = TRUE)))
for(p in cointegratedPortfolio) {
  pos_lambda <- which.max(p$trace@lambda)
  p_length <- length(p$portfolio)
  getOptimalEigenvector <-p$trace@V[1:p_length,pos_lambda]
  prices <- getPrices(p$portfolio)
  timeStamps <- time(p$portfolio[[1]])
  selected = list()
  for(x in colnames(prices)){
    if(!grepl("_Volume",x)) {
      selected[[x]] <- x
    }
  }
  prices <- dplyr::select(prices, as.character(selected))
  portfolioSpread <- rowSums(t(getOptimalEigenvector*t(prices)))
  # This function calculates the halflife of mean reversion and returns the result
  # dy(t) = (lambda*y(t-1) + mu)dt + dE
  # Halflife = -log(2)/lambda
  # Note: the function assumes that the johansen procedure was executed on the Portfolio
  laggedPortfolioSpread = vect_lag(portfolioSpread,1)
  deltaSpread = portfolioSpread-laggedPortfolioSpread
  laggedPortfolioSpread <- laggedPortfolioSpread[!is.na(laggedPortfolioSpread)]
  deltaSpread <- deltaSpread[!is.na(deltaSpread)]
  
  fit <- lm(deltaSpread ~ laggedPortfolioSpread)
  halfLife <- -log(2)/fit$coefficients[2]
  halfLifeString <- paste(" (HalfLife is ",ceiling(halfLife),' days)',sep="")
  currencyString <- colnames(prices)[1]
  spreadString <- paste(getOptimalEigenvector[1], "*", colnames(prices)[1],sep="")
  for(j in 2:p_length)
  {    
    currencyString <- paste(currencyString,colnames(prices)[j],sep="_")
    sign = "-"
    if(getOptimalEigenvector[j] > 0)
      sign = "+"
    spreadString <- paste(spreadString,sign,round(abs(getOptimalEigenvector[j]),2),"*",
                          colnames(prices)[j],sep="")
  }
  filename <- paste0("analyse/", currencyString,".html")
  autoCovar <- paste0(currencyString,"_acf.jpg")
  resSpread <- paste0(currencyString,"_test.jpg")
  johansen <- summary(p$trace)
  dickey_fuller <- adf.test(portfolioSpread)
  write("<pre>", filename, append = TRUE)
  write(johansen@test.name, filename, append = TRUE)
  write.table(cbind(round(johansen@teststat,2), johansen@cval), filename, append = TRUE)
  write.table(round(johansen@V,2), filename, append = TRUE)
  write("</pre><pre>", filename, append = TRUE)
  write(c(dickey_fuller$method,"p-value", dickey_fuller$p.value), filename, append = TRUE)
  write("</pre>", filename, append = TRUE)
  write(paste0("<img src='img/",autoCovar,"'/>"), filename, append = TRUE)
  write(paste0("<img src='img/",resSpread,"'/>"), filename, append = TRUE)
  jpeg(paste0("analyse/img/", autoCovar))
  acf(portfolioSpread)
  dev.off( )
  jpeg(paste0("analyse/img/", resSpread))
  s <- portfolioSpread
  plot(timeStamps,s,xlab=paste("Time",halfLifeString),ylab="Spread",main=spreadString,type="l")
  abline(h=c(mean(s),mean(s)+sd(s),a=mean(s)+2*sd(s),
             mean(s)-sd(s),mean(s)-2*sd(s)),col=c("green","blue","red","blue","red"))
  dev.off( )
}
#








