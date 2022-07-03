# 参数说明：
# S.0   现值
# V     目标期望值
# mu    无风险利率
# sigma 年（月）波动率
# t     单位时长，年或月

#### 现值小于目标值的概率 ####
less.prob<-function(S.0,V,mu,sigma,t){
  d<-(log(S.0/V)+(mu-sigma^2/2))/sigma*sqrt(t)
  result<- pnorm(-d)
  return(result)
}
#### 现值大于目标值的概率 ####
greater.prob<-function(S.0,V,mu,sigma,t){
  d<-(log(S.0/V)+(mu-sigma^2/2))/sigma*sqrt(t)
  result<- 1-pnorm(-d)
  return(result)
}

#### 计算在固定概率下突破的高位值 ####
highValue<- function(S.0,mu,sigma,t,p){
  formula.1<- (mu-sigma^2/2)*2;
  formula.2<-qnorm(p)*sigma*sqrt(t)
  value<-S.0*exp(formula.1-formula.2)
  return(value)
}

#### 计算在固定概率下瀑布的低位值 ####
lowValue<- function(S.0,mu,sigma,t,p){
  formula.1<- (mu-sigma^2/2)*2;
  formula.2<-qnorm(p)*sigma*sqrt(t)
  value<-S.0*exp(formula.1+formula.2)
  return(value)
}
