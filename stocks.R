Sys.setenv(http_proxy="http://127.0.0.1:1087")
Sys.setenv(https_proxy="http://127.0.0.1:1087")
source("volatility.R")

library(quantmod)
library(rugarch)
# 深指sz 沪指ss
data <-getSymbols("600975.ss",src="yahoo",
                 auto.assign = FALSE)

x<-log(data[,4]/data[,1])
n<-length(data);
data<-na.omit(data);
sigma<-historicalVolDay(data[,1],data[,4],n)
vol.year<-sigma*sqrt(250)
print(vol.year)
#res<-forecastVaR(x,"gjrGARCH")
#cat("波动率：",res*100,"%")