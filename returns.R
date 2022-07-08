##########################################
# 本脚本用于输出时间序列的收益率
Sys.setenv(http_proxy="http://127.0.0.1:1087")
Sys.setenv(https_proxy="http://127.0.0.1:1087")
library(quantmod)
library(ggplot2)

# 天康生物、益生股份、新五丰、金新农
# 002100.sz、002458.sz、600975.ss、002548.sz
data<-getSymbols("002548.sz",src="yahoo",
                 from="2018-07-01",
                  auto.assign = FALSE)

set<-NULL;
d<-log(data[,4]/data[,1])*100;
d<-na.omit(d)
print(plot(d,main="002548.sz"))