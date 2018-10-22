library(tseries) 
library(forecast) 
library(dplyr)
library(psych)
ds = read.csv("VIX.csv") 
step =  3
stop = 7
model = c(1,0,1)
start <- as.Date("01-01-18",format="%d-%m-%y")
end   <- as.Date("31-12-18",format="%d-%m-%y")
hedged_pnl <- function(x) { 
  if (as.numeric(x[2])>0) { # up
    if (as.numeric(x[3])>0){ # correct buy signal or stop loss
      return (ifelse(abs(as.numeric(x[5]))<stop,abs(as.numeric(x[2])),-1*stop))
    } else {
      return (ifelse(abs(as.numeric(x[4]))<stop,-1*abs(as.numeric(x[2])),-1*stop))
    }
  } else { # down
    if (as.numeric(x[3])<0){ # correct sell signal or stop loss
      return (ifelse(abs(as.numeric(x[4]))<stop,abs(as.numeric(x[2])),-1*stop))
    } else {
      return (ifelse(abs(as.numeric(x[5]))<stop,-1*abs(as.numeric(x[2])),-1*stop))
    }
  }
}
pnl <- function(x) {
  if(as.numeric(x[2])*as.numeric(x[3])>0) 
    return (abs(as.numeric(x[2])))
  else 
    return(-1*abs(as.numeric(x[2])))
}
indexesdt = as.matrix(ds[,c("Close")])
returns = data.frame(DATE= as.Date(ds[-1,]$Date),
                     ABS_RTRN=diff(ds$Close), PREDICTED=diff(ds$Close),
                     HIGH_RTRN=as.numeric(ds[-1,]$High-ds[-nrow(ds),]$Close),
                     LOW_RTRN=as.numeric(ds[-1,]$Low-ds[-nrow(ds),]$Close))

benchmark <- data.frame(returns) %>%
  filter(DATE < end + step)  %>%
  filter(DATE >= start) 

for(count in 1:nrow(benchmark)) {
  if (count == 1 || count %% step == 1) {
    train = data.frame(returns) %>%
      filter(DATE < benchmark[count,]$DATE)  # %>%
      #  filter(DATE > benchmark[count,]$DATE - 355*2) # 2 years is enough
    test = data.frame(returns) %>%
      filter(DATE >= benchmark[count,]$DATE) %>%
      top_n(n=-1*step,wt=DATE)
    fit = arima(train$ABS_RTRN, order=model) # take bic
    forecasts = predict(fit, step) 
    for(i in 1:step){
      if((count+i-1) <= nrow(benchmark))
        benchmark[count+i-1,]$PREDICTED <- forecasts$pred[i]
    }
  }
}
benchmark$PNL <- apply(benchmark, 1, FUN = pnl)
benchmark$H_PNL <- apply(benchmark, 1, FUN = hedged_pnl)

sum(benchmark %>% filter(PNL != H_PNL) %>% select(PNL)) # where hedge helped
sum(benchmark %>% filter(PNL != H_PNL) %>% select(H_PNL)) # where hedge helped

hist(benchmark$PNL)
plot(benchmark$DATE, benchmark$ABS_RTRN)
lines(benchmark$DATE,benchmark$PREDICTED, col="red", type="p") 
summary(benchmark$PNL)
sum(benchmark$PNL)
sum(benchmark$H_PNL)
sum(benchmark$ABS_RTRN)
w<-count(benchmark%>%filter(ABS_RTRN*PREDICTED > 0)) # winner over looser
l<-count(benchmark%>%filter(ABS_RTRN*PREDICTED <= 0)) # winner over looser
r<-c(w,l,w/(l+w))
r
# ARIMA(1,0,1) can predict VIX movements and 52%-57% times right