library(ggplot2)
library(caTools)
library(dygraphs)
library(xts)
library(forecast)
library(fGarch)
library(tseries)
library(grid)
data<-read.csv("~/Desktop/BTC_tidied.csv",header=TRUE);
da=data.frame(data$Date,data$Closing_Price)
da$data.Date = as.Date(da$data.Date)

klines<-ggplot(data=da, 
           aes(data.Date,as.numeric(data.Closing_Price))) + 
           geom_line() +
           scale_x_date(date_breaks = "3 month") +
           labs(x = "Date",y = "BTC-USDT Price",title = "BTC Price")
da$logPrice= log(as.numeric(da$data.Closing_Price))
logpp<-ggplot(data=da, aes(data.Date,as.numeric(logPrice))) +  geom_line() +
  scale_x_date(date_breaks = "3 month") +
  labs(x = "Date",y = "BTC-USDT Price",title = "BTC Log Price Value")
da$sqrt= sqrt(da$logPrice)
sqrtpp<-ggplot(data=da, aes(data.Date,as.numeric(sqrt))) +  geom_line() +
  scale_x_date(date_breaks = "3 month") +
  labs(x = "Date",y = "BTC-USDT Price",title = "BTC Log Price Sqrt Value")
newFrame=da[-c(1),]
vol<-ggplot(data=newFrame, aes(data.Date,diffPrice)) + geom_line()+
  scale_x_date(date_breaks = "3 month") 
fit = auto.arima(da$sqrt, trace = TRUE, test = "kpss", ic = "bic")
bt<-Box.test(fit$residuals, lag = 12, type = "Ljung-Box")
model=garchFit(~arma(1,0)+garch(1,1), data=diff(da$logPrice), trace=F, cond.dist ='std')

summary (model)
resid=residuals(model, standardize=T)
bt<-Box.test(resid^2)
print(bt)
plot(resid,type='l', main="Residual plot for model") 

pred=predict(model,n.ahead=15,plot=TRUE,crit_val =1)
plot(forecast(fit),xlab="Year",ylab="Stock Value")