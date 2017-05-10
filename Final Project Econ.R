
setwd("C:/Time series econometrics/Final Project")

to <- read.csv("TSITTL.csv", header = TRUE)
fr <- read.csv("TSIFRGHT.csv", header = TRUE)
pa <- read.csv("TSIPSNGR.csv", header = TRUE)

t = to[,2]
f = fr[,2]
p = pa[,2]

t1 = t[1:194]
f1 = f[1:194]
p1 = p[1:194]

model = lm(t1~f1+p1)
summary(model)
re = cbind(1,f2,p2)%*%model$coefficients

t2 = t[195:206]
f2 = f[195:206]
p2 = p[195:206]

source(file="intord.R")
intord(t1)
intord(f1)
intord(p1)

dt = diff(t1)
df = diff(f1)
dp = diff(p1)

maxp <- 11
dtlags <- embed(dt,maxp)
dflags <- embed(df,maxp)
dplags <- embed(dp,maxp)

df1<-cbind(dt,df,dp)


#Try with more lags and choose the best model

dynr1 <- lm(dtlags[,1] ~ dtlags[,2:4]+dflags[,1:4]+dplags[,1:4])

summary(dynr1)

#Seasonality

source(file="seas.R")
n <- length(dtlags[,1])
s <- seas(n,12)

dynr2 <- lm(dtlags[,1] ~ dtlags[,2:4]+dflags[,1:4]+dplags[,1:4]+s$seas[,1:11])
summary(dynr2)


AIC(dynr1)
AIC(dynr2)

# No seasonality is found

#Auto-correlation

# test best (so far) model for serial correlation

library(lmtest)

bgtest(dynr1)
bgtest(dynr1, order = 2)
bgtest(dynr1, order = 3)
bgtest(dynr1, order = 4)
bgtest(dynr1,order = 5)
bgtest(dynr1,order = 6)


# Box-Ljung Q statistic
res = dynr1$resid
b <- Box.test(res,lag = 10, type = "Ljung-Box")
b

# Box-Ljung Q statistic p-values
blt <- rep(0,10)
for (i in 1:10) {
  b <- Box.test(res,lag = i , type = "Ljung-Box")
  blt[i] <- b$p.value
}
blt

# There is serial correlation

# Granger casuality

dynr2 <- lm(dtlags[,1] ~ dtlags[,2:4]+dflags[,2:4]+dplags[,2:4])
dynr3 <- lm(dtlags[,1] ~ dtlags[,2:4]+dflags[,2:4])
dynr4 <- lm(dtlags[,1] ~ dtlags[,2:4]+dplags[,2:4])
dynr5 <- lm(dtlags[,1] ~ dflags[,2:4]+dplags[,2:4])
anova(dynr2,dynr3,test="F")
anova(dynr2,dynr4,test="F")
anova(dynr2,dynr5,test='F')


# There is a no clear granger causality

# Cointegration

cointr <- lm(t~p+f)
summary(cointr)
res1 <- cointr$resid
intord(res1)

res1l = embed(res1,5)
ecmterm<-res1l

# Selecting Lags

library(vars)
z <- cbind(t1,f1,p1) #choosing lag
VARselect(z, lag.max=12, type="const",season=12)

# Let us go for four lags as suggested by AIC

dz = cbind(dt,dp,df)
var3 <- VAR(dz, p=4, type="const",season=12,exogen=res1l[,2])
summary(var3)

p<-5

r1 <- lm(dtlags[,1]~dtlags[,2:p]+dflags[,2:p]+dplags[,2:p]+ecmterm[1:183])
r2 <- lm(dtlags[,1]~dtlags[,2:p]+dflags[,2:p]+ecmterm[1:183])
r3 <- lm(dtlags[,1]~dtlags[,2:p]+dplags[,2:p]+ecmterm[1:183])
r4 = lm(dtlags[,1]~dflags[,2:p]+dplags[,2:p]+ecmterm[1:183])
anova(r2, r1, test="F") 
anova(r3, r1, test="F")
anova(r4, r1, test="F")

r1 = lm(t1~p1+f1+ecmterm[1:194,2:3])
library(lmtest)

bgtest(r1)
bgtest(r1, order = 2)
bgtest(r1, order = 3)
bgtest(r1, order = 4)
bgtest(r1,order = 5)
bgtest(r1,order = 6)


irf2 <- irf(var3, impulse = "dp", response = "dt")
plot(irf2)

irf3 <- irf(var3, impulse = "df", response = "dt")
plot(irf3)

dynr2 <- lm(dtlags[,1] ~ dtlags[,2:4]+dflags[,1:4]+dplags[,1:4])
summary(dynr2)

#forecast

p = 12
d = 0
q = 0
xx = cbind(p1,f1,ecmterm[1:194,2:3])
arma <- arima(t1, order = c(p,d,q),xreg=xx)
xreg=xx
fcast0 <- predict(arma,se.fit = TRUE,xreg=xx)

# Using ECM 

par(mfrow=c(1,1))
pred = cbind(1,p2,f2,ecmterm[191:202,2:3])%*%r1$coefficients
