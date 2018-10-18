library(tseries) 
library(forecast) 
library(dplyr)
library(psych)
ds = read.csv("SP500.csv") 
today = as.Date('2018-1-1')
step = 3
pnl <- function(x) {
  if(as.numeric(x[2])*as.numeric(x[3])>0) 
    return (abs(as.numeric(x[2])))
  else 
    return(-1*abs(as.numeric(x[2])))
}
indexesdt = as.matrix(ds[,c("Close")])
returns = data.frame(DATE= as.Date(ds[-1,]$Date),ABS_RTRN=diff(ds$Close))
start <- as.Date("01-01-18",format="%d-%m-%y")
end   <- as.Date("01-10-18",format="%d-%m-%y")
today <- start
totalPnL <- data.frame(pnl= numeric(0))
while(today <= end) {
  # print(today)
  train = data.frame(returns) %>%
    filter(DATE < today)  %>%
    filter(DATE > '2016-1-1') # 2 years is enough
  test = data.frame(returns) %>%
    filter(DATE >= today) %>%
    top_n(n=-1*step,wt=DATE)
  today <- today + step
  
  fit = arima(train$ABS_RTRN, order=c(1,1,0)) 
  
  forecasts = predict(fit, step) 
  results = data.frame(DATE= as.Date(test$DATE),
                       REAL_RTRN = test$ABS_RTRN, PREDICTED=forecasts$pred)
  results$PNL <- apply(results, 1, FUN = pnl)
  total_pnl <- sum(results$PNL)
  print(total_pnl)
  totalPnL[nrow(totalPnL)+1, ] <- total_pnl
}
sum(totalPnL)
sum(test$ABS_RTRN)