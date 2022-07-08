##########################################
# 本脚本用于输出时间序列的自动定阶
Sys.setenv(http_proxy="http://127.0.0.1:1087")
Sys.setenv(https_proxy="http://127.0.0.1:1087")
library(quantmod)
library(rugarch)
library(forecast)
library(PerformanceAnalytics)
# 天康生物、益生股份、新五丰、金新农
# 002100.sz、002458.sz、600975.ss、002548.sz
data <-getSymbols("002458.sz",src="yahoo",
                  auto.assign = FALSE)
d<-diff(log(data[,4]/data[,1]))*100;
d<-na.omit(d)
fit = auto.arima(d, trace = TRUE, test = "kpss", ic = "bic")
print(fit)