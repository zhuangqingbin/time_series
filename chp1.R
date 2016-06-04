

library(TSA)

# ??1-3

set.seed(20)
n<-100 
p<-0.5 
u0<-rbinom(n,1,p)  
u1<-(u0-0.5)*2 
x<-c(0,cumsum(u1)) 
plot(diff(x),type="l",main="",xlab="时间",ylab="差分值",
     xlim=c(0,100),cex.axis=1.5,cex.lab=1.5)


par(mfrow=c(2,1),cex.axis=1.5,cex.lab=1.5)
acf(x,main="随机游走模型",xlim=c(1,25))
acf(diff(x),main="一阶差分序列",xlim=c(1,25))
par(mfrow=c(1,1))

Box.test(diff(x),lag=5,type = c("Box-Pierce"))
Box.test(diff(x),lag=5,type = c("Ljung-Box"))
#都不能拒绝原假设,认为一阶差分序列不存在相关性




index = ts(read.csv("shindex.csv",header=T)[,2],start = c(1995,1),frequency = 12)
index.dif=diff(log(index))
par(mfrow=c(2,1),cex.axis=1.5,cex.lab=1.5)
acf(as.vector(index),main="上证综指ָ",xlim=c(1,25))
#滞后20期的自相关系数仍显著异于零，初步判断上证综指水平序列为非平稳时间序列。
acf(as.vector(index.dif),main="上证综指的对数差分",xlim=c(1,25))
#对数差分序列的自相关函数快速缩减到零，初步判断对数差分序列为平稳时间序列，滞后2期的单个自相关系数大于临界值，显著异于零，进一步BOX检验
par(mfrow=c(1,1))
Box.test(index.dif,lag=5)
Box.test(index.dif,lag=5,type = c("Ljung-Box"))
#拒绝原假设，对数差分序列不是白噪声过程，存在自相关性
