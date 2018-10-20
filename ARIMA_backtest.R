library(tseries) 
library(forecast) 
library(dplyr)
library(psych)
ds = read.csv("SP500.csv") 
today = as.Date('2018-1-1')
step =  3
stop = 20
hedged_pnl <- function(x) { 
  if (as.numeric(x[1])>0) { # up
    if (as.numeric(x[2])>0){ # correct buy signal or stop loss
      return (ifelse(abs(as.numeric(x[4]))<stop,abs(as.numeric(x[1])),-1*stop))
    } else {
      return (ifelse(abs(as.numeric(x[3]))<stop,-1*abs(as.numeric(x[1])),-1*stop))
    }
  } else { # down
    if (as.numeric(x[2])<0){ # correct sell signal or stop loss
      return (ifelse(abs(as.numeric(x[3]))<stop,abs(as.numeric(x[1])),-1*stop))
    } else {
      return (ifelse(abs(as.numeric(x[4]))<stop,-1*abs(as.numeric(x[1])),-1*stop))
    }
  }
}
pnl <- function(x) {
  if(as.numeric(x[1])*as.numeric(x[2])>0) 
    return (abs(as.numeric(x[1])))
  else 
    return(-1*abs(as.numeric(x[1])))
}
indexesdt = as.matrix(ds[,c("Close")])
returns = data.frame(DATE= as.Date(ds[-1,]$Date),
                     HIGH_RTRN=as.numeric(ds[-1,]$High-ds[-nrow(ds),]$Close),
                     LOW_RTRN=as.numeric(ds[-1,]$Low-ds[-nrow(ds),]$Close),
                     ABS_RTRN=diff(ds$Close))
start <- as.Date("01-01-18",format="%d-%m-%y")
end   <- as.Date("01-10-18",format="%d-%m-%y")
benchmark <- data.frame(returns) %>%
  filter(DATE < end + step)  %>%
  filter(DATE >= start) 
today <- start
results = data.frame(
                     REAL_RTRN = numeric(0), PREDICTED=numeric(0),
                     HIGH_RTRN=numeric(0), LOW_RTRN=numeric(0))
while(today <= end) {
  # print(today)
  train = data.frame(returns) %>%
    filter(DATE < today)  %>%
    filter(DATE > today - 355*2) # 2 years is enough
  test = data.frame(returns) %>%
    filter(DATE >= today) %>%
    top_n(n=-1*step,wt=DATE)
  today <- today + step
  
  fit = arima(train$ABS_RTRN, order=c(1,1,0)) 
  
  forecasts = predict(fit, step) 
  for(i in 1:step){
    results[nrow(results)+1,] <- c(test[i,]$ABS_RTRN,
          forecasts$pred[i],test[i,]$HIGH_RTRN,test[i,]$LOW_RTRN)
  }
}
results$PNL <- apply(results, 1, FUN = pnl)
results$H_PNL <- apply(results, 1, FUN = hedged_pnl)

sum(results$PNL)
sum(results$H_PNL)
#count(totalPnL%>%filter(pnl > 0))
#count(totalPnL%>%filter(pnl <= 0))
#plot(totalPnL)
#hist(totalPnL)
#summary(totalPnL)
sum(benchmark$ABS_RTRN)
#count(benchmark%>%filter(ABS_RTRN > 0))
#count(benchmark%>%filter(ABS_RTRN <= 0))
# View(viewtotalPnL %>% filter(PNL != HPNL))