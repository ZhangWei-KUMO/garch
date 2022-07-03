#######################
# 估计一只股票的β系数尤为重要
# 它是衡量真实回报率的重要指标
# 无风险利率
R.f<- 0.027
# 预期超额回报率
e<- 0.05

realR<-function(rho,sigma,sigma.m){
  beta<-rho*sigma/sigma.m
  R.r<-R.f+e*beta
  return(R.r)
}

