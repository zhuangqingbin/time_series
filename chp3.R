


library(TSA)
library(astsa)
library(zoo)
library(forecast)



ex<-read.zoo("exchange.csv",sep=",",header=T, format = "%Y-%m-%d")
exr<-diff(ex)/lag(ex,k=-1)*100;

par(mfrow=c(2,1),cex.axis=1.5,cex.lab=1.5)
plot(ex,type="l",xlab="",ylab="",main="人民币汇率走势图")
plot(exr,type="l",xlab="",ylab="",main="人民币汇率的变化率")
par(mfrow=c(1,1))


par(mfrow=c(2,1),cex.axis=1.5,cex.lab=1.5)
acf(ts(exr),30,main="")
pacf(ts(exr),30,main="")
par(mfrow=c(1,1))
#人民币对美元汇率变化率序列的偏自相关函数为一阶截尾，根据AR模型的PACF性质，可以考虑AR(1),MA(1)也成立，可以用AIC选择模型




CPI<-ts(read.csv("cpi.csv",header=T)[,2],start=1978)
CPI.dif<-diff(CPI)

par(mfrow=c(2,1),cex.axis=1.5,cex.lab=1.5)
plot(CPI,type="o",xlab="年份")
plot(CPI.dif,type="o",xlab="年份")
par(mfrow=c(1,1))


par(mfrow=c(2,1),cex.axis=1.5,cex.lab=1.5)
acf(CPI.dif)
pacf(CPI.dif)
par(mfrow=c(1,1))
#居民消费价格指数差分序列的自相关函数在二阶后截尾，偏自相关函数在四阶后截尾，考虑MA(2)




set.seed(12)
arma11.sim <- arima.sim(list(order = c(1,0,1), ar = c(-0.6), ma = c(-0.8)), n = 200)
sim.eacf<- eacf(arma11.sim,ar.max = 7,ma.max = 7)
print(sim.eacf$eacf,digits = 2)





gdp<-ts(read.csv("gdp.csv",header=T)[,2],start = 1952)
gdp.r=diff(log(gdp))
par(mfrow=c(2,1),cex.axis=1.5,cex.lab=1.5)
plot(gdp,type="o",xlab="年份",ylab="",main="人均国内生产总值ֵ")
plot(gdp.r,type="o",xlab="年份",ylab="",main="人均国内生产总值的对数差分")
par(mfrow=c(1,1))
gpd.eacf = eacf(gdp.r)
#除了在(1,6)位置的EACF显著异于零之外，(1,1)的楔形区域都不显著，(1,6)位置0.274只是稍微大于临界值为1.96/sqrt(55)=0.264


#模型pq的识别
set.seed(12)
arma11.sim <- arima.sim(list(order = c(1,0,1), ar = c(-0.6), ma = c(-0.8)), n = 200)

aic<-auto.arima(arma11.sim,max.p=6,max.q=12,ic="aic")
aic
aicc<-auto.arima(arma11.sim,max.p=6,max.q=12,ic="aicc")
aicc
bic<-auto.arima(arma11.sim,max.p=6,max.q=12,ic="bic")
bic



set.seed(20)
estimate.ma1.mom=function(x){
  r=acf(x,plot=F)$acf[1]; if (abs(r)<0.5) return((1-sqrt(1-4*r^2))/(2*r))
  else return(NA)
}
#模拟次数
M<-3000
ma.mom<-rep(0,M)
wn.var.mom <-rep(0,M)
for (i in 1:M){
  ma.sim <- arima.sim(list(order = c(0,0,1), ma = c(-0.7)), n = 100)
  ma.mom[i] <- estimate.ma1.mom(ma.sim)
  wn.var.mom[i] <- var(ma.sim)/(1+(estimate.ma1.mom(ma.sim))^2)
}
#直方图
par(mfrow=c(1,2),cex.axis=1.5,cex.lab=1.5)
hist(ma.mom,xlab=expression(theta))
hist(wn.var.mom,xlab=expression(sigma^2))
M-length(ma.mom[ma.mom>1])


#利用矩法估计AR(1)模型
ex <-ts(read.csv("exchange.csv",header=T)[,2])
exr <- diff(ex)/lag(ex,k=-1)*100;
#样本统计量
acf(exr,lag.max=1,plot=FALSE)
mean(exr)
var(exr)
ar.yw(exr,order.max=1,AIC=F)  

#用最小二乘法估计参数
ex <-ts(read.csv("exchange.csv",header=T)[,2])
exr <- diff(ex)/lag(ex,k=-1)*100;
arima(exr,order=c(1,0,0),method='CSS') 


#用极大似然法估计参数
ex <-ts(read.csv("exchange.csv",header=T)[,2])
exr <- diff(ex)/lag(ex,k=-1)*100;
arima(exr,order=c(1,0,0),method='ML')


#模型诊断
ex <-ts(read.csv("exchange.csv",header=T)[,2])
exr <- diff(ex)/lag(ex,k=-1)*100;
exr.ar<-sarima(exr,1,0,0,details=F) 
exr.ar
Box.test(residuals(exr.ar$fit),lag=10,type=c("Ljung-Box"),fitdf=1)
#Box检验中的df设为1，由结果可知残差不存在自相关性，图形显示了AR(1)模型的标准化残差图形和残差诊断图，可以看出标准化残差偏离正态分布，但残差的单一自相关系数和多个自相关系数联合为零的Ljing-Box统计量不显著，因此模型是充分的



exr.ma<-sarima(exr,0,0,1,details=F)
exr.ma
Box.test(residuals(exr.ma$fit),lag=10,type=c("Ljung-Box"),fitdf=1)
#MA(1)的AIC比AR(1)略高




gdp<- ts(read.csv("gdp.csv",header=T)[,2],start = 1952)
gdp.r<-diff(log(gdp))
arma.fit<-sarima(gdp.r,1,0,1,details=F)
arma.fit
Box.test(residuals(arma.fit$fit),lag=10,type=c("Ljung-Box"),fitdf=2)
#残差不存在自相关性



#预测

ex<-ts(read.csv("exchange.csv",header=T)[,2])
exr<-diff(ex)/lag(ex,k=-1)*100;
exr.for<-sarima.for(exr,15,1,0,0)
exr.for$pred
exr.for$se


gdp <- ts(read.csv("gdp.csv",header=T)[,2],start = 1952)
gdp.r<-diff(log(gdp))
gdp.r.for<-sarima.for(gdp.r,10,1,0,1)
gdp.r.for$pred
gdp.r.for$se
