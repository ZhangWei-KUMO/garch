##########################################
# 本脚本用于输出股票的收益率分布,可确定该股票处于T分布还是正态分布
Sys.setenv(http_proxy="http://127.0.0.1:1087")
Sys.setenv(https_proxy="http://127.0.0.1:1087")
library(quantmod)
library(rugarch)
library(PerformanceAnalytics)
# 天康生物、益生股份、新五丰、金新农
# 002100.sz、002458.sz、600975.ss、002548.sz
data <-getSymbols("002100.sz",src="yahoo",
                  auto.assign = FALSE)
d<-diff(log(data[,4]/data[,1]));
d<-na.omit(d)
pacf(eu^2, main="")
