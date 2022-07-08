Sys.setenv(http_proxy="http://127.0.0.1:1087")
Sys.setenv(https_proxy="http://127.0.0.1:1087")
library(quantmod)
library(rugarch)
data <-getSymbols("002100.sz",src="yahoo",
                  from="2018-07-01",
                  auto.assign = FALSE)
data<-na.omit(data)

returns<-log(data[,4]/data[,1])
returns<-na.omit(returns)
mod.spec=ugarchspec(mean.model=list(armaOrder=c(1,1)),
                    variance.model=
                      list(model="sGARCH",
                           garchorder=c(1,1)),
                    distribution.model="norm")
modelfit=ugarchfit(data=returns,spec=mod.spec,out.sample = 20)
print(modelfit)
