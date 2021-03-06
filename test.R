source("prob.R")
source("basic.R")
t<-1;
# 当前价格
S.0 <- 47738
# 目标价
V <- 15000
# 连续无风险复利
mu<-0.03
# 交易日 按年计算传统金融产品为265，数字货币为365；
# 按月计算则除以12
# 波动率
sigma<- 0.0408*sqrt(365)
# 计算概率密度
x<-lowValue(S.0,0.03,sigma,1,0.5)
print(x)

val<-qnorm(0.01)
print(0.1836*val)