###########
# 构建频数直方图，可以看出收益率的左偏和右偏
# 以及分布均值
Sys.setenv(http_proxy="http://127.0.0.1:1087")
Sys.setenv(https_proxy="http://127.0.0.1:1087")
library(PerformanceAnalytics)
library(quantmod)
data <-getSymbols("600975.ss",src="yahoo",
                     from="2016-01-01",
                     to="2022-7-5",
                     auto.assign = FALSE)
return<-CalculateReturns(data$"600975.SS.Adjusted")
chart.Histogram(return,method=c("add.density","add.normal"),
                colors=c("blue","red","green"))
m<-mean(log(data$"600975.SS.Close"/data$"600975.SS.Open"));
print(head(data))
print(m)