library(forecast) 
library(tseries) 
library(urca) 
library(xts) 
library(dplyr)
library(vars)
library(PerformanceAnalytics)
# Lag function for matrices
# Note: Function was foudn online and it was copied here
lag.matrix <- function(x, k=1){
  N <- ncol(x)
  l <- matrix(embed(x,k+1)[, -c(1:(k*N))], ncol=N)
  NAs <- matrix(rep(NA, k*N), ncol=N)
  rbind(NAs, l)
}
# R equivalent of matlab repmat
# Note: Function was found online and was copied here
repmat = function(X,m,n){
  ##R equivalent of repmat (matlab)
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}
# Repeats the last non NA value. Keep leading NA
# Note: Function was found online and it was copied here
fillMissingValues = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)        # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
} 
# Note: Function was found online and it was copied here
vect_lag <- function(v, n=1, forward=FALSE) {
  if (forward)
    c(v[(n+1):length(v)], rep(NA, n))
  else
    c(rep(NA, n), v[1:(length(v) - n)])
}
getPrices = function(theObject) {
  prices <- data.frame(theObject[[1]])
  prices['Date'] <- time(theObject[[1]])
  # colnames(prices) <- colnames(toTest[[1]])
  for(i in 2:length(theObject)) {
    nxt <- data.frame(theObject[[i]])
    nxt['Date'] <- time(theObject[[i]])
    # colnames(nxt) <- colnames(toTest[[i]])
    prices <- merge(prices,nxt,by='Date')
    #prices <- cbind(prices,nxt)
  }
  selected = list()
  for(x in colnames(prices)){
    if(!grepl("_Volume",x)) {
      selected[[x]] <- x
    }
  }
  #  prices <- prices %>% 
  #    filter_all(all_vars(abs(.) > 0))
  prices <- dplyr::select(prices, as.character(selected))
  return (prices)
}
# 1. load
data = "fx"
# data = "IG/csv"
sep = "/"
from = "2008-01-01"
to = "2014-06-01"
max_port = 4
leverage <- 10000
c_entryZscore = 1 # Entry deviation
c_exitZscore = 0 # Exit deviation
portfolio <- list()
for (ccy in  list.files(data)) {
  print (paste("loading ", ccy))
  dat = read.csv(paste(data,ccy, sep=sep), header=T) 
  dat<- dat[dat$Volume>0,]
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
    keep <- list() 
    nam <- colnames(prices)
    for(i in nam) {
      if (i != 'Date') {
        keep[[i]] <- i
      }
    }
    prices<- prices[ , as.character(keep), drop = TRUE]
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
if (isTRUE(dir.exists("analyse/img"))) {
  do.call(file.remove, list(list.files("analyse/img", full.names = TRUE)))
} else {
  dir.create("analyse/img")
}
for(p in cointegratedPortfolio) {
  pos_lambda <- which.max(p$trace@lambda)
  p_length <- length(p$portfolio)
  getOptimalEigenvector <-p$trace@V[1:p_length,pos_lambda]
  prices <- getPrices(p$portfolio)
  timeStamps<-prices[,'Date']
  keep <- list() 
  nam <- colnames(prices)
  for(i in nam) {
    if (i != 'Date') {
      keep[[i]] <- i
    }
  }
  prices<- prices[ , as.character(keep), drop = TRUE]
  
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
  print(currencyString)
  filename <- paste0("analyse/", currencyString,".html")
  autoCovar <- paste0(currencyString,"_acf.jpg")
  resSpread <- paste0(currencyString,"_test.jpg")
  resMult <- paste0(currencyString,"_mult.jpg")
  resHist <- paste0(currencyString,"_hist.jpg")
  resEq <- paste0(currencyString,"_eq.jpg")
  johansen <- summary(p$trace)
  dickey_fuller <- adf.test(portfolioSpread)
  #if (dickey_fuller$p.value > 0.02) {
  #  next()
  #}
  write("<pre>", filename, append = TRUE)
  write(johansen@test.name, filename, append = TRUE)
  write.table(cbind(round(johansen@teststat,2), johansen@cval), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(round(johansen@V,2), filename, row.names = FALSE, col.names = FALSE,  append = TRUE)
  write("</pre><pre>", filename, append = TRUE)
  write(c(dickey_fuller$method,"p-value", dickey_fuller$p.value), filename, append = TRUE)
  write("</pre>", filename, append = TRUE)
  write(paste0("<img src='img/",autoCovar,"'/>"), filename, append = TRUE)
  write(paste0("<img src='img/",resSpread,"'/>"), filename, append = TRUE)
  write(paste0("<img src='img/",resMult,"'/>"), filename, append = TRUE)
  write(paste0("<img src='img/",resHist,"'/>"), filename, append = TRUE)
  write(paste0("<img src='img/",resEq,"'/>"), filename, append = TRUE)
  jpeg(paste0("analyse/img/", autoCovar))
  acf(portfolioSpread)
  dev.off( )
  jpeg(paste0("analyse/img/", resSpread))
  s <- portfolioSpread
  plot(timeStamps,s,xlab=paste("Time",halfLifeString),ylab="Spread",main=spreadString,type="l")
  abline(h=c(mean(s),mean(s)+sd(s),a=mean(s)+2*sd(s),
             mean(s)-sd(s),mean(s)-2*sd(s)),col=c("green","blue","red","blue","red"))
  dev.off( )
  jpeg(paste0("analyse/img/", resMult))
  plot(timeStamps, prices[,1], ylim=c(0.5,1.5)*range(prices), main="",type="l",ylab="",xlab="")
  for(i in 2:length(prices)) {
    lines(timeStamps, prices[,i], col=i)
  }
  legend("topleft", legend = colnames(prices), 
         text.col=c(1:length(prices)), ncol=length(prices) )
  dev.off( )
  
  # 5 execute strategy
  meanSpread <- mean(portfolioSpread)
  stdSpread <- sd(portfolioSpread)
  zScore = (portfolioSpread - meanSpread)/stdSpread
  longsEntry <- (zScore < -c_entryZscore)
  longsExit <- (zScore > -c_exitZscore)
  shortsEntry <- (zScore > c_entryZscore)
  shortsExit <- (zScore < c_exitZscore)
  
  nrDataPoints <- length(portfolioSpread)
  # Create vector with 0 values
  numUnitsLong <- vector(mode="numeric",length=nrDataPoints)
  # Set NA from 2:end
  numUnitsLong[2:nrDataPoints] <- NA
  numUnitsShort <- numUnitsLong # Copy by value
  
  # numUnitsLong represents an array with 1 entrys on positions where we are long the portfolio spread
  numUnitsLong[longsEntry] = 1;
  numUnitsLong[longsExit] = 0
  numUnitsLong <- fillMissingValues(numUnitsLong)
  
  # numUnitsShort represents an array with -1 entrys on positions where we are short the portfolio spread
  numUnitsShort[shortsEntry] <- -1
  numUnitsShort[shortsExit] <- 0
  numUnitsShort <- fillMissingValues(numUnitsShort)
  
  # numUnits represents an array that indicates how many units of the portfolio spread we bought (1), or sold (-1)
  numUnits <- numUnitsLong+numUnitsShort
  # numUnitsPortfolio epresents a matrix with the number of units of the portfolio that we buy or sell, for every timestamp
  numUnitsPortfolio <- repmat(matrix(numUnits),1,length(p$portfolio))
  # hedgeRatioMatrix represents a matrix with the hedgeRatio of each individual currencyPair, for every timestamp 
  # This can also be viewed as the "shares allocation" for each currency Pair at any given point in time (the hedge ratio is fixed and always the same)
  hedgeRatioMatrix <- repmat(matrix(getOptimalEigenvector,nrow=1),length(numUnits),1)
  # prices matrix represents a matrix with the prices of the CurrencyPairs in the Portfolio, for every timestamp
  pricesMatrix <- data.matrix(prices)
  # hedgeRatioMatrix * pricesMatrix represents the USD capital allocation to buy the portfolio
  # positions represents our USD capital in each currencyPair at any given point in time
  positions <- numUnitsPortfolio*hedgeRatioMatrix*pricesMatrix
  # takePosition contains a 1 value at timestamps where we open a position (and incur transaction costs)
  takePosition = vector(mode="numeric",length=nrDataPoints)
  for(i in 2:length(numUnits))
  {
    if(abs(numUnits[i])==1 & numUnits[i-1]==0)
      takePosition[i]=1
  }
  
  # Pnl of the strategy on each timeStamp
  pnl <- lag.matrix(positions)*(pricesMatrix-lag.matrix(pricesMatrix))/lag.matrix(pricesMatrix)
  pnl[which(is.na(pnl))] <- 0 # First entry is NA. Set to 0.
  pnl <- rowSums(pnl)
  absTransactionCosts <- 0.995
  pnl <- pnl * absTransactionCosts
  # Return is P&L divided by gross market value of the portfolio
  laggedPositions <- lag.matrix(positions)
  laggedPositions[which(is.na(laggedPositions))] <- 0
  ret <- pnl/rowSums(abs(laggedPositions))
  ret[which(is.na(ret))] <- 0
  
  # Calculate and plot the results
  thedate = as.Date(timeStamps, format = "%Y.%m.%d %H:%M:%S") 
  APR <- round((prod(1+ret)^(252/length(ret))-1)*100,digits=4)
  sharpeRatio <- round(sqrt(252)*mean(ret)/sd(ret),digits=4)
  results <- (cumprod(1+ret)-1)*100
  maxDD <- round(maxDrawdown(as.xts(ret,thedate))*100,digits=4)
  jpeg(paste0("analyse/img/", resHist))
  dailyReturnsString <- paste('Daily Returns (average: ',round(mean(ret*100),digits=4), '%, std: ', round(sd(ret*100),digits=4), '%)',sep="")
  plot(timeStamps,ret*100,xlab="Time",ylab="Returns (%)", main=dailyReturnsString, type='h')
  dev.off()
  jpeg(paste0("analyse/img/", resEq))
  resultString <- paste('(APR: ',APR,'%, SR: ',sharpeRatio,', MAX DD: ',maxDD,'%)',sep="")
  plot(timeStamps,results,xlab="Time",ylab="Return (%)",main=resultString, type='l')
  dev.off()
  
  # asb return cashflows
  abs_ret <- leverage*pricesMatrix-leverage*lag.matrix(pricesMatrix)
  abs_ret[which(is.na(abs_ret))] <- 0 # First entry is NA. Set to 0.
  sum_abs_ret <- as.matrix(abs_ret) %*% as.vector(getOptimalEigenvector)
  sum_abs_ret <- numUnits * sum_abs_ret
  write(paste('<br/>On ',leverage, ' leverage absulte return is $', round(sum(sum_abs_ret))), 
        filename, append = TRUE)
}









