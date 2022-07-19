##############################
# 波动性预测
Sys.setenv(http_proxy="http://127.0.0.1:1087")
Sys.setenv(https_proxy="http://127.0.0.1:1087")
library(quantmod)
library(rugarch)
library(forecast)
library(xts)

data <-getSymbols("002548.sz",src="yahoo",
                  from="2018-07-01",
                  auto.assign = FALSE)
data<-na.omit(data)
# 对数收益率
#returns<-log(data[,4]/data[,1])*100
# 市场收益率
returns<-diff(log(AdjClose))*100
returns<-na.omit(returns)

n<-length(returns);
train.num<-round(n*0.9);
# 将原数据分割成训练数据和测试数据两份
train.data<-returns[1:train.num];
test.data<-returns[(train.num+1):n];
## 获取时间轴列表
date.list<-index(returns)[(train.num+1):n];

mod.spec=ugarchspec(mean.model=list(armaOrder=c(1,0)),
                    variance.model=
                      list(model="eGARCH",
                           garchorder=c(1,1)),
                    distribution.model="sstd")
# 基于训练数据做模型拟合
modelfit=ugarchfit(data=train.data,spec=mod.spec,solver="solnp")
fspec <- getspec(modelfit)
setfixed(fspec) <- as.list(coef(modelfit))
m<-length(test.data)-1;
pred <- ugarchforecast(fspec, data = test.data,
                       n.ahead = 10, 
                       # n.roll与out.sample数值应当一样大
                       n.roll = m,
                       out.sample = m)
print(sigma(pred))
#print(exp(cumsum(Forecast_Data_values/100)))
#print(fitted(pred))
plot(pred,which=2)

