historicalVolDay<-function(Open_Price,Closing_Price,n){
  m<-n+1
  return_rate<-log(Closing_Price/Open_Price);
  mu.sum<-sum(return_rate)
  mu.square.sum<-sum(return_rate^2)
  s<-sqrt(mu.square.sum/n -mu.sum^2/(n*m))
  return(s)
}