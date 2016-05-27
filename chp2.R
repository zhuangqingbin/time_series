setwd("c:\\")

# ??2-1

GDP <- ts(read.csv("gdpq.csv",header=T),start=1992,freq=4)
gdp.decom <- decompose(GDP,type="multiplicative")

plot(gdp.decom,type="o",cex.axis=1.5,cex.lab=1.5)
Trend <- gdp.decom$trend    #长期趋势
Seasonal <- gdp.decom$seasonal   #季节因素
Random <- gdp.decom$random    #随机项


# ??2-2

hs300 <- read.csv("hs300.csv",header = T)[,2]

plot(hs300,type="l",cex.axis=1.5,cex.lab=1.5)
ma20 <- filter(ts(hs300),filter=rep(1/20,20),sides=1)
lines(ma20,lwd=2)

# ??2-3

GDP <- ts(read.csv("gdpq.csv",header=T),start=1992,freq=4)
trendpattern1<-filter(GDP,filter = c(1/8, 1/4, 1/4, 1/4, 1/8), sides=2)
plot(GDP,type="o",cex.axis=1.5,cex.lab=1.5,xlab="ʱ??",ylab="GDP")
lines(trendpattern1)


m1 <- filter (GDP, filter =rep(1/4,4), sides=1)
m2 <- filter (m1, filter =rep(1/4,4), sides=1)
trendpattern2=2*m1-m2
plot(GDP,type="o",cex.axis=1.5,cex.lab=1.5,xlab="ʱ??",ylab="GDP")
lines(trendpattern2)

# ??2-4

consumer_conf<-ts(read.csv("consumer_cf.csv",header=T)[,2], start = c(2010, 1), freq = 12)
plot(consumer_conf,type="o",cex.axis=1.5,cex.lab=1.5)
cf.hw1 <- HoltWinters(consumer_conf, beta=F, gamma=F)
cf.hw1

cf.hw2 <- HoltWinters(consumer_conf, alpha = 0.5, beta=F, gamma=F)
cf.hw2

cf.hw1f <- predict(cf.hw1, 6,prediction.interval = TRUE)
plot(cf.hw1,cf.hw1f,type="o",cex.axis=1.5,cex.lab=1.5)

# ??2-5


GDP <- ts(read.csv("gdpq.csv",header=T),start=1992,freq=4)
GDP.hw <- HoltWinters(GDP,seasonal="multi")
plot(GDP.hw$fitted,type="o",cex.axis=1.5,cex.lab=1.5)
plot(GDP.hw,type="o",cex.axis=1.5,cex.lab=1.5)

GDP.for <- predict(GDP.hw, n.ahead=4 * 5)
ts.plot(GDP, GDP.for, type="o",lty = 1:2)
GDP.for
