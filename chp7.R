
library(FinTS)
library(rugarch)




hs300 <- ts(read.csv("hs300.csv",header = T))[,2]
            
# 简单收益率
n = length(hs300)
hs300.ret = hs300[2:n]/hs300[1:n-1] - 1
# 五日收益率
hs300.ret.5 = hs300[6:n]/hs300[1:c(n-5)] - 1

# 连续复利收益率
hs300.lret = log(1 + hs300.ret)
hs300.lret = log(hs300[2:n]/hs300[1:n-1])


par(mfrow=c(2,2),cex.axis=1.5,cex.lab=1.5)
plot(hs300,type="l")
plot(hs300.lret,type="l")
plot(abs(hs300.lret),type="l")
plot(hs300.lret^2,type="l")
par(mfrow=c(1,1))
#可以发现，股市波动表现为大的波动跟随大的波动，小的波动跟随小的波动，这种特征为波动聚集性



#检验ARCH效应
acf(hs300.lret^2,xlim=c(1,25),cex.axis=1.5,cex.lab=1.5)
#FinTS包中的ArchTest函数做ARCH拉格朗日乘数检验
ArchTest(hs300.lret)
#LM36.517，拒绝不存在ARCH效应的原假设
Box.test(hs300.lret^2,lag=12,type = "Ljung-Box")
Box.test(hs300.lret,lag=12,type = "Ljung-Box")
#由BOX检验，可知收益率序列本身在滞后12期的情况下没有显著的相关性

# ??7-8
#根据沪深300指数收益率平方的自相关函数和偏相关函数，尝试建立ARCH(7)模型，通过ugarchspec函数设定ARCH模型形式，通过ugarchfit拟合设定的模型
arch.spec = ugarchspec(variance.model = list(garchOrder=c(7,0)), 
                       mean.model = list(armaOrder=c(0,0)))
arch.fit = ugarchfit(spec=arch.spec, data=hs300.lret,
                     solver.control=list(trace = 0))
show(arch.fit)


#检验ARCH(7)模型的标准化残差，不能拒绝没有ARCH效应的零假设，所拟合的ARCH(7)是充分的
hs.res = ts(residuals(arch.fit,standardize=T))
ArchTest(hs.res)



# ??7-9

garch11.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)))
garch11.fit = ugarchfit(spec=garch11.spec, data=hs300.lret, solver.control=list(trace = 0))
show(garch11.fit)
hs.res = ts(residuals(garch11.fit,standardize=T))
ArchTest(hs.res)

info = cbind (infocriteria(arch.fit),infocriteria(garch11.fit))
colnames(info,do.NULL = FALSE)
colnames(info) <- c("ARCH","GARCH")
info

#??7-10
cp <- ts(read.csv("s600004.csv",header = T))[,2]
n = length(cp)
ret = log(cp[2:n]/cp[1:n-1])
ArchTest(ret)
gjrgarch.spec = ugarchspec(variance.model = list(model="gjrGARCH",garchOrder=c(1,1)), 
                             mean.model = list(armaOrder=c(0,0)))
gjrgarch.fit = ugarchfit(spec=gjrgarch.spec, data=ret)
show(gjrgarch.fit)
res = ts(residuals(gjrgarch.fit,standardize=T))
ArchTest(res)

egarch.spec = ugarchspec(variance.model = list(model="eGARCH"), 
                         mean.model = list(armaOrder=c(0,0)))
egarch.fit = ugarchfit(spec=egarch.spec, data=ret)
show(egarch.fit)
res = ts(residuals(egarch.fit,standardize=T))
ArchTest(res)

garch.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)))
garch.fit = ugarchfit(spec=garch.spec, data=ret, solver.control=list(trace = 0))
nic.garch11 = newsimpact(garch.fit)
nic.egarch11 = newsimpact(egarch.fit)
nic.gjrgarch11 = newsimpact(gjrgarch.fit)

par(mfrow=c(1,3),cex.axis=1.5,cex.lab=1.5)
plot(nic.garch11$zx, type="l", main="GARCH(1,1)", 
     nic.garch11$zy, ylab=nic.garch11$yexpr, xlab=nic.garch11$xexpr)
plot(nic.egarch11$zx, type="l",  main="EGARCH(1,1)", 
     nic.egarch11$zy, ylab=nic.egarch11$yexpr, xlab=nic.egarch11$xexpr)
plot(nic.gjrgarch11$zx, type="l", main="TGARCH(1,1)", 
     nic.gjrgarch11$zy, ylab=nic.gjrgarch11$yexpr, xlab=nic.gjrgarch11$xexpr)
par(mfrow=c(1,1))


# ??7-11

garch.m.spec=ugarchspec(variance.model=list(model="sGARCH"),
                        mean.model=list(armaOrder=c(0,0),archm=TRUE,archpow=2))
garch.m = ugarchfit(spec=garch.m.spec,data=ret, solver.control=list(trace = 0))
show(garch.m)


