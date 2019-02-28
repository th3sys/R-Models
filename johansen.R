library(forecast) 
library(tseries) 
library(urca) 
library(xts) 
library(dplyr)
library(vars)

file <- 'inputData_EWA_EWC_IGE.csv'
dat = read.csv(file, header=T) 
res <- ca.jo(dat[,2:3])
summary(res)