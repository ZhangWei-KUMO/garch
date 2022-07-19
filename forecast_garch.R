Sys.setenv(http_proxy="http://127.0.0.1:1087")
Sys.setenv(https_proxy="http://127.0.0.1:1087")
library(quantmod)
library(rugarch)
library(forecast)
library(xts)

data <-getSymbols("002548.sz",src="yahoo",
                  from="2020-07-01",
                  auto.assign = FALSE)
data<-na.omit(data)

returns<-log(data[,4]/data[,1])*100
returns<-na.omit(returns)

n<-length(returns);
train.num<-round(n*0.9);
# 将原数据分割成训练数据和测试数据两份
train.data<-returns[1:train.num];
test.data<-returns[(train.num+1):n];
## 获取时间轴列表
date.list<-index(returns)[(train.num+1):n];

mod.spec=ugarchspec(mean.model=list(armaOrder=c(1,0)),
                    variance.model=list(model="sGARCH",garchorder=c(1,1)),
                    distribution.model="sstd")
mod.spec.2=ugarchspec(mean.model=list(armaOrder=c(1,0)),
                    variance.model=list(model="eGARCH",garchorder=c(1,1)),
                    distribution.model="sstd")
mod.spec.3=ugarchspec(mean.model=list(armaOrder=c(1,0)),
                    variance.model=list(model="sGARCH",garchorder=c(1,1)),
                    distribution.model="sstd")
# 基于训练数据做模型拟合
modelfit=ugarchfit(data=train.data,spec=mod.spec,solver="solnp")
modelfit.2=ugarchfit(data=train.data,spec=mod.spec.2,solver="solnp")
modelfit.3=ugarchfit(data=train.data,spec=mod.spec.3,solver="solnp")

fspec <- getspec(modelfit)
setfixed(fspec) <- as.list(coef(modelfit))

fspec.2 <- getspec(modelfit.2)
setfixed(fspec.2) <- as.list(coef(modelfit.2))

m<-length(test.data)-1;
pred <- ugarchforecast(fspec, data = test.data,
                       n.ahead = 1, 
                       n.roll = m,
                       out.sample = m)
pred.2 <- ugarchforecast(fspec.2, data = test.data,
                       n.ahead = 1, 
                       n.roll = m,
                       out.sample = m)
#VaR.predict <- as.numeric(quantile(pred, probs = 0.05)) 
VaR.predict <- as.numeric(sigma(pred))
VaR.predict.2 <- as.numeric(sigma(pred.2))
varts<- xts(x=VaR.predict, order.by=as.POSIXct(date.list))
varts.2<- xts(x=VaR.predict.2, order.by=as.POSIXct(date.list))

pp<-plot(
  cbind(varts,varts.2, returns),
  col = c("red", "orange","black"),
  type = "l", xlab = "dates",
  main="002548.sz gjrGARCH",
  ylab = "Volatility")

print(pp)

