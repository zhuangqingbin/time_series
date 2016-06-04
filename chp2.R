

GDP <- ts(read.csv("gdpq.csv",header=T),start=1992,freq=4)
gdp.decom <- decompose(GDP,type="multiplicative")

plot(gdp.decom,type="o",cex.axis=1.5,cex.lab=1.5)
Trend <- gdp.decom$trend    #长期趋势
Seasonal <- gdp.decom$seasonal   #季节因素
Random <- gdp.decom$random    #随机项


# 简单移动平均法计算20日移动平均线
hs300 <- read.csv("hs300.csv",header = T)[,2]

plot(hs300,type="l",cex.axis=1.5,cex.lab=1.5)
ma20 <- filter(ts(hs300),filter=rep(1/20,20),sides=1)
lines(ma20,lwd=2)

# 中心化移动平均
GDP <- ts(read.csv("gdpq.csv",header=T),start=1992,freq=4)
trendpattern1<-filter(GDP,filter = c(1/8, 1/4, 1/4, 1/4, 1/8), sides=2)
plot(GDP,type="o",cex.axis=1.5,cex.lab=1.5,xlab="ʱ??",ylab="GDP")
lines(trendpattern1)


# 二次移动平均法
m1 <- filter (GDP, filter =rep(1/4,4), sides=1)
m2 <- filter (m1, filter =rep(1/4,4), sides=1)
trendpattern2=2*m1-m2
plot(GDP,type="o",cex.axis=1.5,cex.lab=1.5,xlab="ʱ??",ylab="GDP")
lines(trendpattern2)

# 简单指数平滑法
consumer_conf<-ts(read.csv("consumer_cf.csv",header=T)[,2], start = c(2010, 1), freq = 12)
plot(consumer_conf,type="o",cex.axis=1.5,cex.lab=1.5)
cf.hw1 <- HoltWinters(consumer_conf, beta=F, gamma=F)
cf.hw1

cf.hw2 <- HoltWinters(consumer_conf, alpha = 0.5, beta=F, gamma=F)
cf.hw2
#预测
cf.hw1f <- predict(cf.hw1, 6,prediction.interval = TRUE)
plot(cf.hw1,cf.hw1f,type="o",cex.axis=1.5,cex.lab=1.5)



# Holt线性指数平滑法，在不考虑季节变动部分的情况下，Holt线性指数平滑法假定了时间序列数据存在一个比较固定的线性趋势
GDP <- ts(read.csv("gdpq.csv",header=T),start=1992,freq=4)
GDP.hw <- HoltWinters(GDP,seasonal="multi")
plot(GDP.hw$fitted,type="o",cex.axis=1.5,cex.lab=1.5)
plot(GDP.hw,type="o",cex.axis=1.5,cex.lab=1.5)

GDP.for <- predict(GDP.hw, n.ahead=4 * 5)
ts.plot(GDP, GDP.for, type="o",lty = 1:2)
GDP.for
