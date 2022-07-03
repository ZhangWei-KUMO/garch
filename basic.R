# 计算N年资产的贴现值
discount<- function(value,interest,t){
  return(value*exp(-interest)*t)
}