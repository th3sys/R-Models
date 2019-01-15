library(forecast) 
library(tseries) 
library(urca) 
library(xts) 
yieldDat = read.table("treasury_yields.txt", header=T) 
date = as.Date(yieldDat[,1], format = "%m/%d/%y") 
dat = as.xts(yieldDat[,3:7], date) 
# 1. regression approach
res = residuals(lm(dat[,3]~dat[,1]+dat[,2]+dat[,4]+dat[,5]))
fitted = fitted(lm(dat[,3]~dat[,1]+dat[,2]+dat[,4]+dat[,5]))
plot(dat)
result <- cbind(fitted=fitted, original=dat[,3], residuals=res)
colnames(result)
plot(result$residuals)
acf(result$residuals)
# not sure if residuals are stationary because acf does not colapse to 0 fast enough
# so run Phillips–Ouliaris test
po.test(dat[,c(3,1,2,4,5)])
# p-value is small so series are stationary but with a long memory
# 2. Vector Error Correction Models
# simulated with φ1 = 0.5,φ2 = 0.55, andλ = 1. A
# total of 5,000 observations were simulated, but, for visual clarity
# only every 10th observation is plotted
n = 5000 
set.seed(12345) 
a1 = 0.5
a2 = 0.55 
lambda = 1 
y1 = rep(0,n) 
y2 = y1 
e1 = rnorm(n) 
e2 = rnorm(n) 
for (i in 2:n){ 
  y1[i] = y1[i-1] + a1 * (y1[i-1] - lambda*y2[i-1]) + e1[i] 
  y2[i] = y2[i-1] + a2 * (y1[i-1] - lambda*y2[i-1]) + e2[i] 
  }
# A VECM was fit to the bond yields using R’s ca.jo() function



