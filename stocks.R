Sys.setenv(http_proxy="http://127.0.0.1:1087")
Sys.setenv(https_proxy="http://127.0.0.1:1087")
library(quantmod)
# 深指sz 沪指ss
data <-getSymbols("002607.sz",src="yahoo",
                 from="2018-01-01",
                 auto.assign = FALSE)
print(median(data[,2]))
chartSeries(data)