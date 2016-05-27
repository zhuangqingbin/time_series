setwd("c:\\")
library(TSA)
library(zoo)
library(urca)

# 计算单位根检验统计量的临界值

qunitroot(c(0.01,0.05,0.10),N=100,trend="nc",statistic="t")
qunitroot(c(0.01,0.05,0.10),N=100,trend="c",statistic="t")
qunitroot(c(0.01,0.05,0.10),N=100,trend="ct",statistic="t")

# 例6-1

fut <- read.zoo("futures.csv",sep=",",header=T,format="%Y/%m/%d")
fut.log=log(fut)
fut.dif = diff(fut.log)


par(mfrow=c(2,2),cex.axis=1.5,cex.lab=1.5)
plot(fut.log)
acf(ts(fut),xlim=c(1,25))
plot(fut.dif)
acf(ts(fut.dif),xlim=c(1,25))
par(mfrow=c(1,1))

fut.adf.c=ur.df(as.vector(fut.log),type=c("drift"),lags=20,selectlags = c("AIC"))
summary(fut.adf.c)

fut.adf=ur.df(as.vector(fut.log),type=c("none"),lags=20,selectlags = c("AIC"))
summary(fut.adf)

fut.ldif = diff(fut.log)
fut.ldif.adf=ur.df(as.vector(fut.ldif),type=c("none"),lags=20,selectlags = c("AIC"))
summary(fut.ldif.adf)


# 例6-2
fut <- read.zoo("futures.csv",sep=",",header=T,format="%Y/%m/%d")
fut.log=log(fut)
fut.pp = ur.pp(as.vector(fut.log), type='Z-tau', model='constant',lags='long')
fut.pp@teststat
fut.pp@cval

fut.ldif = diff(fut.log)
fut.ldif.pp = ur.pp(as.vector(fut.ldif), type='Z-tau', model='constant',lags='long')
fut.ldif.pp@teststat
fut.ldif.pp@cval


# 例6-3

iav <- ts(read.csv("iav.csv",header=T)[,2])

plot(iav,cex.axis=1.5,cex.lab=1.5)
iav.kpss <- ur.kpss(iav, type = "mu",lags = "long")
summary(iav.kpss)


# 例6-4
ci <- ts(read.csv("spot_futures.csv",header=T)[,2:3])
spot.log = log(ci[,1])
fut.log=log(ci[,2])
spot.adf=ur.df(spot.log,type=c("none"),lags=20,selectlags = c("AIC"))
spot.adf@teststat
spot.adf@cval
spot.dif.adf=ur.df(diff(spot.log),type=c("none"),lags=20,selectlags = c("AIC"))
spot.dif.adf@teststat
spot.dif.adf@cval

coin <- lm(spot.log~ fut.log)
coin

resid.adf = ur.df(coin$residuals)
resid.adf@teststat
resid.adf@cval
ci.log = cbind(spot.log,fut.log)
po.test(ci.log,demean=F)

# 例6-5
ci <- ts(read.csv("spot_futures.csv",header=T)[,2:3])
ci.log = cbind(log(ci[,1]),log(ci[,2]))
colnames(ci.log) = c('spot.log','fut.log')
ci.dif = ts(embed(diff(ci.log), dim=2))
colnames(ci.dif) = c('sp.d', 'fut.d', 'sp.d1', 'fut.d1')
eq = summary(lm(spot.log~ fut.log, data=ci.log))
error.ecm = ts(resid(eq)[-c(1,length(resid(eq)))])
ecm.reg1 = lm(sp.d ~ error.ecm + sp.d1 + fut.d1,data=ci.dif)
summary(ecm.reg1)
ecm.reg2 = lm(fut.d ~ error.ecm + sp.d1 + fut.d1,data=ci.dif)
summary(ecm.reg2)
