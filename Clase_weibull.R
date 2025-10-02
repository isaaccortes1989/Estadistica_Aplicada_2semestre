library(gamlss) 

y <- scan()
370 1055 1270 1502 1763
706 1085 1290 1505 1768
716 1102 1293 1513 1781
746 1102 1300 1522 1782
785 1108 1310 1522 1792
797 1115 1313 1530 1820
844 1120 1315 1540 1868
855 1134 1330 1560 1881
858 1140 1355 1567 1890
886 1199 1390 1578 1893
886 1200 1416 1594 1895
930 1200 1419 1602 1910
960 1203 1420 1604 1923
988 1222 1420 1608 1940
990 1235 1450 1630 1945
1000 1238 1452 1642 2023
1010 1252 1475 1674 2100
1016 1258 1478 1730 2130
1018 1262 1481 1750 2215
1020 1269 1485 1750 2268
2440


summary(y)
boxplot(y,horizontal = T)
hist(y,main="",xlab="Tiempos de falla",ylab="Frecuencia")
box()


#GAMMA-WEIBULL-NORMAL-EXPONENCIAL-LOGNORMAL
help(gamlss)

fit <- gamlss(y~1,family = EXP)
summary(fit)
exp(7.2449)
#lambda = 1400.942
#AIC =  1667.465
#BIC =  1670.08
plot(fit)

fit2 <- gamlss(y~1,family = WEI)
summary(fit2)
exp(7.34347)
exp(1.37282)
plot(fit2)
#AIC = 1496.153 
#BIC = 1501.383 

fit3 <- gamlss(y~1,family = GA)
summary(fit3)
exp(7.2449)
exp(-1.23638)
plot(fit3)
#AIC = 1498.541
#BIC = 1503.772

fit4 <- gamlss(y~1,family=LOGNO)
summary(fit4)
#mu = 7.20211
exp(-1.18924)
plot(fit4)
#AIC = 1505.226
#BIC = 1510.456

fit5 <- gamlss(y~1,family=RG)
summary(fit5)
#mu = 7.20211
exp(5.91685)
plot(fit5)
#AIC = 1505.648
#BIC = 1510.878 


ks.test(fit2$residuals,"pnorm",mean(fit2$residuals),sd(fit2$residuals))

x <- seq(0,2500,0.0001)
z <- dWEI(x,mu=exp(7.34347),sigma=exp(1.37282))

hist(y,main="",xlab="Tiempos de falla",ylab="Frecuencia",freq=F)
box()
lines(x,z,col="red")

#R(t) = 1-F(t) F(t) es la acumulada
1-pWEI(2000,mu=exp(7.34347),sigma=exp(1.37282))
