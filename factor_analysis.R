######################
#主成分分析法，以求出财报中最好的股票
library(psych)
library(tidyverse)
x<-read.csv("factors.csv")
# 获取企业名称
names<-return.share<-x[[1]]
return.share<-x[[2]]
return.all<-x[[8]]
income<-x[[9]]
profit<-x[[10]]
df<-data.frame(
           return.all,income,profit,return.share
           )
#calculate principal components
results <- prcomp(df, scale = TRUE)
biplot(results, scale = 0)

# 旋转成分矩阵
results$rotation <- -1*results$rotation
# 进行检验，KMO需要一直减到大于0.6
kt<-KMO(df)
bt<-bartlett.test(df)
# 旋转
print(results$rotation)
# 获取主成分得分
f <- -1*results$x
total<-f[,1]*63.22/89.25+f[,2]*26.03/89.25
# 取PC1和PC2作为主因子
rank<-data.frame(
  names=names,
  PC1=f[,1],
  PC2=f[,2],
  total
)
# 根据条件进行排序
print(rank[order(-rank$PC1),])
var_explained<-results$sdev^2 / sum(results$sdev^2)
qp<-qplot(c(1:9), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)
