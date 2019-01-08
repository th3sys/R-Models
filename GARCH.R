library(tseries) 
library(forecast) 
library(dplyr)
library(rugarch) 
ds = read.csv("VIX.csv") 
today = as.Date("01-01-18",format="%d-%m-%y")
days = 30
indexesdt = as.matrix(ds[,c("Close")])
returns = data.frame(DATE= as.Date(ds[-1,]$Date),
                     ABS_RTRN=diff(ds$Close))
#PERCENT_RTRN=diff(indexesdt)/indexesdt[-nrow(indexesdt),])
train = data.frame(returns) %>%
  filter(DATE < today) 
test = data.frame(returns) %>%
  filter(DATE >= today) %>%
  top_n(n=-days,wt=DATE)
# looking for signs of mean reversion
plot(train, type="l")
# acf decays to zero in periodic cycles
acf(train$ABS_RTRN)
# p-value smaller than 0.01
adf.test(train$ABS_RTRN) 

# FIT
arma.garch.norm = ugarchspec(mean.model=list(armaOrder=c(1,1,1)), 
                             variance.model=list(garchOrder=c(1,1))) 
fit.arma.garch.norm = ugarchfit(train$ABS_RTRN, spec=arma.garch.norm, out.sample=days) 
show(fit.arma.garch.norm)
#acf(residuals(fit.arma.garch.norm)) 

pnl <- function(x) {
  if(as.numeric(x[2])*as.numeric(x[3])>0) 
    return (abs(as.numeric(x[2])))
  else 
    return(-1*abs(as.numeric(x[2])))
}
# PREDICT
forecasts = ugarchforecast(fit.arma.garch.norm, n.ahead=days/10, n.roll=9)
results = data.frame(DATE= as.Date(test$DATE),
              REAL_RTRN = test$ABS_RTRN, 
              PREDICTED=as.vector(forecasts@forecast$seriesFor))
results$PNL <- apply(results, 1, FUN = pnl)
total_pnl <- sum(results$PNL)
total_pnl
plot(results$DATE,results$REAL_RTRN, 
     ylim=c(min(train$ABS_RTRN),max(train$ABS_RTRN)), type="p") 
lines(results$DATE,results$PREDICTED, col="red", type="p") 
