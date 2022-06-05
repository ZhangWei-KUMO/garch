library(ggplot2)
library(caTools)
library(dygraphs)
library(xts)
library(forecast)
library(fGarch)
library(tseries)

data<-read.csv("~/Desktop/btc_tidied.csv",header=TRUE);
da=data.frame(data$Date,data$Closing_Price)
da$data.Date = as.Date(da$data.Date)
pp<-ggplot(data=da, 
           aes(data.Date,as.numeric(data.Closing_Price))) + 
           geom_line() +
           scale_x_date(date_breaks = "3 month") +
           labs(x = "Date",y = "BTC-USDT Price",title = "BTC Price")
da$logPrice= log(as.numeric(da$data.Closing_Price))
logpp<-ggplot(data=da, aes(data.Date,as.numeric(logPrice))) + geom_line()
da$sqrt= sqrt(da$logPrice)
sqrtpp<-ggplot(data=da, aes(data.Date,as.numeric(sqrt))) + geom_line()
diffPrice=diff(da$logPrice)
acf(diffPrice)
newFrame=da[-c(1),]

vol<-ggplot(data=newFrame, aes(data.Date,diffPrice)) + geom_line()+
  scale_x_date(date_breaks = "3 month") 
adft<-adf.test(diffPrice)
fit1 = auto.arima(da$sqrt, trace = TRUE, test = "kpss", ic = "bic")
# 检验残差项
bt<-Box.test(fit1$residuals, lag = 10, type = "Ljung-Box")
acf(fit1$residuals^2)
tsdisplay(fit1$residuals)
tsdiag(fit1)
model=garchFit(~arma(1,0)+garch(1,1), data=diff(da$sqrt), trace=F, cond.dist ='std')

print(summary (model))
# 预测未来波动区间
pred=predict(model,n.ahead=10,plot=TRUE,crit_val =1)
# 下限
lowerbound = pred$lowerInterval;
lowpred = diffinv(lowerbound, xi= 3.021804)
#
actuallow = exp(lowpred^2)
# 中间值
midbound = pred$meanForecast;
#
valpred = diffinv(midbound, xi= 3.021804)
#
actualval = exp(valpred^2)
# 上限
upperbound = pred$upperInterval
uppred = diffinv(upperbound, xi= 3.021804)

actualup = exp(uppred^2)

actPrice = data$Price[1300:length(data$Price)]

lPrice = c(actPrice,actuallow)
mPrice=c(actPrice,actualval)
hPrice=(c(actPrice,actualup))

plot.ts(lPrice, main="Forecast plot for Bitcoin for 20 days",col="red")
lines(1:length(hPrice),hPrice,col="Green")
lines(1:length(mPrice),mPrice,col="Blue")