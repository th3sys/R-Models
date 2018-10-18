library(tseries) 
library(forecast) 
library(dplyr)
SP500 = read.csv("SP500.csv") 
today = as.Date('2018-1-1')
indexesdt = as.matrix(SP500[,c("Close")])
returns = data.frame(DATE= as.Date(SP500[-1,]$Date),
                     ABS_RTRN=diff(SP500$Close))
                     #PERCENT_RTRN=diff(indexesdt)/indexesdt[-nrow(indexesdt),])
train = data.frame(returns) %>%
  filter(DATE < today)  %>%
  filter(DATE > '2016-1-1')
test = data.frame(returns) %>%
  filter(DATE >= today) %>%
  top_n(n=-3,wt=DATE)
# looking for signs of mean reversion
plot(train, type="l")
# acf decays to zero in periodic cycles
acf(train$ABS_RTRN)
# p-value smaller than 0.01
adf.test(train$ABS_RTRN) 
auto.arima(train$ABS_RTRN, max.P=0, max.Q=0, ic="aic")
# ARIMA(1,1,0) with drift 
auto.arima(train$ABS_RTRN, max.P=0, max.Q=0, ic="bic")
# ARIMA(1,1,0) with drift 

# FIT
fit = arima(train$ABS_RTRN, order=c(1,1,0)) 
acf(residuals(fit)) 

pnl <- function(x) {
  if(as.numeric(x[2])*as.numeric(x[3])>0) 
    return (abs(as.numeric(x[2])))
  else 
    return(-1*abs(as.numeric(x[2])))
}
# PREDICT
forecasts = predict(fit, 3) 
results = data.frame(DATE= as.Date(test$DATE),
                     REAL_RTRN = test$ABS_RTRN, PREDICTED=forecasts$pred)
results$PNL <- apply(results, 1, FUN = pnl)
total_pnl <- sum(results$PNL)
total_pnl
plot(results$DATE,results$REAL_RTRN, 
     ylim=c(min(train$ABS_RTRN),max(train$ABS_RTRN)), type="p") 
lines(results$DATE,results$PREDICTED, col="red", type="p") 
lines(results$DATE, 
      forecasts$pred + forecasts$se, col="blue") 
lines(results$DATE,
      forecasts$pred - forecasts$se, col="blue")

