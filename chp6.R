
library(TSA)
library(zoo)
library(urca)


qunitroot(c(0.01,0.05,0.10),N=100,trend="nc",statistic="t")
qunitroot(c(0.01,0.05,0.10),N=100,trend="c",statistic="t")
qunitroot(c(0.01,0.05,0.10),N=100,trend="ct",statistic="t")

# ??6-1

fut <- read.zoo("futures.csv",sep=",",header=T,format="%Y/%m/%d")
fut.log=log(fut)
fut.dif = diff(fut.log)


par(mfrow=c(2,2),cex.axis=1.5,cex.lab=1.5)
plot(fut.log)
acf(ts(fut),xlim=c(1,25))
plot(fut.dif)
acf(ts(fut.dif),xlim=c(1,25))
par(mfrow=c(1,1))

fut.adf.a=ur.df(as.vector(fut.log),type=c("none"),lags=20,selectlags = c("AIC"))
summary(fut.adf.a)

fut.adf.b=ur.df(as.vector(fut.log),type=c("drift"),lags=20,selectlags = c("AIC"))
summary(fut.adf.b)

fut.adf.c=ur.df(as.vector(fut.log),type=c("trend"),lags=20,selectlags = c("AIC"))
summary(fut.adf.c)
#tau统计量来检验adf检验中的一阶滞后项系数，原假设是为0，就是存在单位根，序列不平稳
#phi1统计量来检验回归方程deltx_{t}=beta_{1}+pi*x_{t-1}+siama{p}deltax_{t-j}+epusilon_{t},原假设beta_{1}=p=0
#对于回归方程deltx_{t}=beta_{1}+beta_{2}*t+pi*x_{t-1}+siama{p}deltax_{t-j}+epusilon_{t},phi2统计量来检验原假设beta_{1}=p=beta_{2}=0，phi3统计量来检验p=beta_{2}=0






fut.ldif = diff(fut.log)
fut.ldif.adf=ur.df(as.vector(fut.ldif),type=c("none"),lags=20,selectlags = c("AIC"))
summary(fut.ldif.adf)


#pp检验。原假设是序列非平稳
fut <- read.zoo("futures.csv",sep=",",header=T,format="%Y/%m/%d")
fut.log=log(fut)
fut.pp = ur.pp(as.vector(fut.log), type='Z-tau', model='constant',lags='long')
fut.pp@teststat
fut.pp@cval

fut.ldif = diff(fut.log)
fut.ldif.pp = ur.pp(as.vector(fut.ldif), type='Z-tau', model='constant',lags='long')
fut.ldif.pp@teststat
fut.ldif.pp@cval


#KPSS检验，原假设是序列平稳

iav <- ts(read.csv("iav.csv",header=T)[,2])

plot(iav,cex.axis=1.5,cex.lab=1.5)
iav.kpss <- ur.kpss(iav, type = "mu",lags = "long")
summary(iav.kpss)



# 协整检验，，要先确定序列同阶单整
ci <- ts(read.csv("spot_futures.csv",header=T)[,2:3])
spot.log = log(ci[,1])
fut.log=log(ci[,2])
spot.adf=ur.df(spot.log,type=c("none"),lags=20,selectlags = c("AIC"))
spot.adf@teststat
spot.adf@cval
spot.dif.adf=ur.df(diff(spot.log),type=c("none"),lags=20,selectlags = c("AIC"))
spot.dif.adf@teststat
spot.dif.adf@cval
#沪深300指数为1阶单证序列，期货价格已经验证过是一阶单整

#下面用EG两步协整检验检验
##先建立回归模型
coin <- lm(spot.log~ fut.log)
coin
##然后检验回归模型的残差序列是不是平稳
resid.adf = ur.df(coin$residuals)
resid.adf@teststat
resid.adf@cval


#可以用po.test检验协整关系，原理也是先回归，然后对残差做pp检验
#原假设是残差是单位根过程，即不协整
ci.log = cbind(spot.log,fut.log)
po.test(ci.log,demean=F)

# 误差修正模型
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
