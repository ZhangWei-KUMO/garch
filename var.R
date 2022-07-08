# data 为对数收益率
# type:"gjrGARCH","eGARCH","sGARCH"...
forecastVaR<-function(data,type){
  n<-as.numeric(length(data));
  dividing<-round(n*0.9);
  m<- n-dividing;
  model<-ugarchspec(variance.model = list(model = type, garchOrder = c(1, 1)), 
                    mean.model = list(armaOrder = c(1, 1), include.mean = FALSE), 
                    distribution.model = "sstd")
  # 进行模型拟合
  modelfit<-ugarchfit(spec=model,data=data)
  spec = getspec(modelfit);
  # 获取GARCH模型估计参数
  setfixed(spec) <- coef(modelfit);
  # 对测试数据进行预测
  test.data<- data[dividing:n,drop=FALSE]*100
  # 向前一步预测
  pred = ugarchforecast(spec, n.ahead = 1, n.roll = m-1, 
                            data =test.data , out.sample = m-1);
  sigma(pred);
  fitted(pred)
  VaR.predict <- as.numeric(quantile(pred, probs = 0.05))
  print(plot(test.data))
  return(tail(VaR.predict, n=1))
}

historyTest<-function(data){
  spec = ugarchspec(mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                    variance.model = list(model = "gjrGARCH"), distribution.model = "sstd")
  fit = ugarchfit(spec, data = x)
  spec2 = spec
  setfixed(spec2)<-as.list(coef(fit))
  filt = ugarchfilter(spec2, x)
  VaR = fitted(filt) + sigma(filt)*qdist("sstd", p=0.05, mu = 0, sigma = 1, 
                                         skew  = coef(fit)["skew"], shape=coef(fit)["shape"])
  print(tail(VaR))
  actual = dji30ret[1001:2500,1]
  
  VaRTest(alpha = 0.05, actual, VaR, conf.level = 0.95)
}