
library(zoo)
library(TSA)
library(astsa)

set.seed(20)
len <- 100
drift <- 0.5
x0 <- drift+rnorm(len)
x<-cumsum(x0)
trd <- 1:100
x.lm <- lm(x~trd)
summary(x.lm)
#R方很高，说明了随机游走暗含了一个线性趋势，所以趋势变量的回归系数非常显著
par(mfrow=c(3,1),cex.axis=1.5,cex.lab=1.5)
plot(x,type="o",main="原序列和拟合序列") 
lines(fitted.values(x.lm))
plot(resid(x.lm),type="o",main="残差")
acf(resid(x.lm),main="残差自相关函数")
par(mfrow=c(1,1))
#图中表明，随机游走模型中的残差仍然存在系统性变动特征，特别是残差的自相关图表明，随机游走模型时间序列对线性趋势回归后残差并不是白噪声过程，存在显著自相关性
#例子表明，分析非平稳时间序列需要对数据所表现出来的趋势也就是产生非平稳的原因进行很严谨的研究，因为如果对趋势进行不适当的估计，得到的时间序列可能会产生人为的波动和自相关性，而不是真正的经济因素


set.seed(20)
len <- 100
drift <- 0.15
x1 <- cumsum(rnorm(len))#无漂移随机游走模型
x2 <- cumsum(drift+rnorm(len))#有漂移随机游走模型

par(mfrow=c(2,2))
acf(x1,main="无漂移随机游走模型")
pacf(x1,main="无漂移随机游走模型")
acf(x2,main="有漂移随机游走模型")
pacf(x2,main="有漂移随机游走模型")
par(mfrow=c(1,1))



x1.dif <- diff(x1)
x2.dif <- diff(x2)
par(mfrow=c(2,2))
acf(x1.dif,main="无漂移随机游走模")
pacf(x1.dif,main="无漂移随机游走模")
acf(x2.dif,main="有漂移随机游走模型")
pacf(x2.dif,main="有漂移随机游走模型")
par(mfrow=c(1,1))


set.seed(20)
phi <- 0.8
theta <- -0.5
x1 <- arima.sim(list(order=c(1,1,0),ar=phi),n=500)
x2 <- arima.sim(list(order=c(0,1,1),ma=theta),n=500)
x3 <- arima.sim(list(order=c(1,1,1),ar=phi,ma=theta),n=500)
par(mfrow=c(3,2),cex.axis=1.5,cex.lab=1.5)
acf(x1,xlim=c(1,25),main=expression(paste("(1-0.8L)(1-L)"~x[t]~"="~epsilon[t])))
pacf(x1,xlim=c(1,25),main=expression(paste("(1-0.8L)(1-L)"~x[t]~"="~epsilon[t])))
acf(x2,xlim=c(1,25),main=expression(paste("(1-L)"~x[t]~"=(1-0.5L)"~epsilon[t])))
pacf(x2,xlim=c(1,25),main=expression(paste("(1-L)"~x[t]~"=(1-0.5L)"~epsilon[t])))
acf(x3,xlim=c(1,25),main=expression(paste("(1-0.8L)(1-L)"~x[t]~"=(1-0.5L)"~epsilon[t])))
pacf(x3,xlim=c(1,25),main=expression(paste("(1-0.8L)(1-L)"~x[t]~"=(1-0.5L)"~epsilon[t])))
par(mfrow=c(1,1))

x1.dif <- diff(x1)
x2.dif <- diff(x2)
x3.dif <- diff(x3)
par(mfrow=c(3,2),cex.axis=1.5,cex.lab=1.5)
acf(x1.dif,xlim=c(1,25),main=expression(paste("(1-0.8L)(1-L)"~x[t]~"="~epsilon[t])))
pacf(x1.dif,xlim=c(1,25),main=expression(paste("(1-0.8L)(1-L)"~x[t]~"="~epsilon[t])))
acf(x2.dif,xlim=c(1,25),main=expression(paste("(1-L)"~x[t]~"=(1-0.5L)"~epsilon[t])))
pacf(x2.dif,xlim=c(1,25),main=expression(paste("(1-L)"~x[t]~"=(1-0.5L)"~epsilon[t])))
acf(x3.dif,xlim=c(1,25),main=expression(paste("(1-0.8L)(1-L)"~x[t]~"=(1-0.5L)"~epsilon[t])))
pacf(x3.dif,xlim=c(1,25),main=expression(paste("(1-0.8L)(1-L)"~x[t]~"=(1-0.5L)"~epsilon[t])))
par(mfrow=c(1,1))


#ARIMA模型的预测
trade <- ts(read.csv(file="trade.csv",head=TRUE)[,2],start=1950)
trade.log <- log(trade)
par(mfrow=c(2,1),cex.axis=1.5,cex.lab=1.5)
plot(trade.log,type="o")
acf(trade.log)
par(mfrow=c(1,1))

trade.dif <- diff(trade.log)
par(mfrow=c(3,1),cex.axis=1.5,cex.lab=1.5)
plot(trade.dif,type="o")
acf(trade.dif)
pacf(trade.dif)
par(mfrow=c(1,1))

ma.fit <- sarima(trade.log,0,1,1,details = F)
ma.fit
trade.for <- sarima.for(trade.log,5,0,1,1)
trade.for


