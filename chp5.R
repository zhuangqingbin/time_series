

library(astsa)
library(TSA)

# ??5-1

cpi <- ts(read.csv("sxcpim.csv",header = T)[,2],freq=12,start=c(2003,1)) 
par(mfrow=c(2,1),cex.axis=1.5,cex.lab=1.5)
plot(cpi,type="o",main="陕西省月度CPI时序图")
acf(as.vector(cpi),lag.max=60,main="")
par(mfrow=c(1,1))
#图形显示CPI数据没有明显的趋势，但存在明显的季节依赖性特征，并且自相关函数并没有出现快速衰减，说明需要进行季节差分得到平稳时间序列

cpi.dif <- diff(cpi,lag=12)
par(mfrow=c(3,1),cex.axis=1.5,cex.lab=1.5)
plot(cpi.dif,type="o",main="陕西省月度CPI的季节差分序列图")
acf(as.vector(cpi.dif),lag.max=60,main="")
pacf(as.vector(cpi.dif),lag.max=60,main="")
par(mfrow=c(1,1))

cpi.fit <- sarima(cpi,0,0,0,P=0,D=1,Q=1,S=12,details=F)
cpi.fit

Box.test(resid(cpi.fit$fit),type=c("Ljung-Box"),lag=10,fit=1)

# ??5-2 
igp <- ts(read.csv("igp.csv",header = T)[,2],freq=12,start=c(2003,1))
par(mfrow=c(2,1),cex.axis=1.5,cex.lab=1.5)
plot(igp,type="o",main="工业总产值ֵ")
acf(as.vector(igp),lag.max=36,main="")
par(mfrow=c(1,1))
#图形显示，工业总产值具有趋势性，自相关函数缓慢衰减，说明原序列为非平稳时间序列

igp.dif <- diff(igp)
par(mfrow=c(2,1),cex.axis=1.5,cex.lab=1.5)
plot(igp.dif,type="o",main="工业总产值的一阶差分")
acf(as.vector(igp.dif),36,main="")
par(mfrow=c(1,1))
#经过一阶差分后，可以看出上升的趋势消失，但季节自相关依然缓慢衰减，所以还有必要进行季节差分消除非平稳性
igp.sdif <- diff(igp.dif,lag=12)
par(mfrow=c(3,1),cex.axis=1.5,cex.lab=1.5)
plot(igp.sdif,type="o",main="工业总产值的一阶差分和季节差分")
acf(as.vector(igp.sdif),lag.max=60,main="")
pacf(as.vector(igp.sdif),lag.max=60,main="")
par(mfrow=c(1,1))
#经过常差分和季节差分后，工业总产值差分序列已经成为平稳时间序列，发现季节周期以及季节周期前后世纪存在显著的自相关和偏相关函数，且自相关函数和偏相关函数拖尾，因此可以尝试建立SARIMA(0,1,1)*(1,1,0)12拟合工业总产值序列

par(mfrow=c(3,1),cex.axis=1.5,cex.lab=1.5)
igp.fit <- sarima(igp,0,1,1,P=1,D=1,Q=0,S=12,details=F)
igp.fit

Box.test(resid(cpi.fit$fit),type=c("Ljung-Box"),lag=10,fit=2)

# ??5-3
igp <- ts(read.csv("igp2.csv",header = T)[,2],freq=12,start=c(2003,1))
igp.insample <- window(igp, start=c(2003, 1), end=c(2011,12))
par(mfrow=c(1,1),cex.axis=1.5,cex.lab=1.5)
for1 <- sarima.for(igp.insample, 5, 0,1,1,1,1,0,12)
for1$pred
leg.names <- c("实际值","预测值ֵ")
ts.plot(igp,for1$pred,gpars=list(lty=c(1,2)))
legend(x="topleft",leg.names,lty=c(1,2))
