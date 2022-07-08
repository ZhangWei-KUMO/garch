omega<- 0.000002;
alpha<- 0.04;
beta<- 0.94;
sigma<-0.013;
t<-20;

a<-log(1/(alpha+beta));

expectedVolatility<- function(omega,alpha,beta,sigma,t,a){
  v.0<- sigma^2;
  v.l<-omega/(1-alpha-beta)
  result<-sqrt((v.l + (1-exp(-a*t))/(a*t)*(v.0-v.l))*252)
  return(result)
}

print(expectedVolatility(omega,alpha,beta,sigma,t,a))