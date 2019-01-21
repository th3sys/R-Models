library(forecast) 
library(tseries) 
library(urca) 
library(xts) 
# The data set midcapD.ts.csv has daily log returns on 20 midcap stocks in columns 2–21.
# To find stock prices from the returns: Pt = P0 exp(r 1 + · · · + rt)
midcapD.ts = read.csv("midcapD.ts.csv",header=T) 
x = midcapD.ts[,2:11] 
prices= exp(apply(x,2,cumsum)) 
options(digits=3) 
summary(ca.jo(prices))
# find cointegrated pairs of assets
# r = 0  | 58.39  59.0 62.42 68.6
# test value of 58.39 is smaller than 10% interval so series are not cointegrated
# 2. use yield different maturities
mk.maturity = read.csv("mk.zero2.csv", header=T) 
yields.cajo = ca.jo(mk.maturity[,2:11])
summary(yields.cajo)
Z =  as.matrix(mk.maturity[,2:11]) %*% yields.cajo@V[,1]
ts.plot(Z)
acf(Z)
# 3. Coke Pepsi
CokePepsi = read.table("CokePepsi.csv", header=T) 
ts.plot(CokePepsi)
ts.plot(CokePepsi[,2] - CokePepsi[,1])
summary(ca.jo(CokePepsi))

# 4. 
Stock_FX_Bond = read.csv("Stock_FX_Bond.csv", header=T) 
adjClose = Stock_FX_Bond[,seq(from=3, to=21, by=2)] 
ts.plot(adjClose) 
summary(ca.jo(adjClose[,c("GM_AC","F_AC")] ))
ts.plot(adjClose[,c("GM_AC","F_AC")] )
summary(ca.jo(adjClose, K=8))
ts.plot(adjClose[,"GM_AC"] - 0.350*adjClose[,"F_AC"] )










