# 仅考虑波动性风险的质押率测算
t<-250
Z.alpha<- qnorm(0.01);
sigma<-0.0403;
H.1<-1+2/3*sqrt(t)*Z.alpha*sigma;
cat(H.1*100,"%")