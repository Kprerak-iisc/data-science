library(readxl)
gdp <- read_excel("C:/Users/prera/Desktop/KP_IISC/MG222/Assignment4/gdp.xlsx")
names(gdp)
gdptr<-ts(gdp$"Trade,Hotels,Transport&Communication",delta=1/4)
gd<-gdptr[1:66]
t <-1:66
plot(c(1,66),range(c(gd)),type="n",main="Quarterly GDP of Trade, Hotels, Transport & Communication sector of India from 1996-97 Q1 to 2012-13 Q2",xlab="Quarters",ylab="GDP i")
lines(t,gd) 
points(t,gd,pch=20,col=2)
decomposed.gdptr<-decompose(gdptr) 
points(t,decomposed.gdptr$trend,col=0)
lines(t,decomposed.gdptr$trend,col=4)
gdptrans<-gdptr
q1<-gdptrans[seq(1,66,4)]
q2<-gdptrans[seq(2,66,4)]
q3<-gdptrans[seq(3,66,4)] 
q4<-gdptrans[seq(4,66,4)]
plot(c(1996,2012),range(gdptrans),type="n",xlab="Year",ylab="GDP.",main="Quarterwise GDP of Trade, Hotels, Transport & Communication sector",font.main=1,cex.main=1.5)
lines(1996:2012,q1) 
lines(1996:2012,q2,col=2) 
lines(1996:2011,q3,col=3) 
lines(1996:2011,q4,col=4) 
legend(1997,6000,legend=c("Q1","Q2","Q3","Q4"),lty=rep(1,4),col=1:4)
plot(decomposed.gdptr)

#Naive Modeling-I: Linear Model of log-THC with Seasonal dummies
lgdp<-log(gd)
w2<-pi
w1<-pi/2
s1<-cos(w1*t)
s2<-cos(w2*t)
s3<-sin(w1*t)
model1_reg<-lm(lgdp~t+s1+s2+s3)
summary(model1_reg)
model2<-lm(lgdp~t+s1+s3)
summary(model2)
library(lmtest)
bptest(model2)
normtest(residuals(model2))
plot(c(1,66),range(lgdp),type="n",main="Regression Model",
       xlab="Quarters",ylab="log-GDP in Crores of Rs.",font.main=1,cex.main=1.5)
lines(1:66,lgdp,lty=1)
points(1:66,lgdp)
lines(t,6.4104817+0.0334384*t+0.0392087*s1-0.0237282*s3,col=2)
boxplot(residuals(model2))
plot(model2)
#second order
tsq<-t^2
model3<-lm(lgdp~t+tsq+s1+s2+s3)
summary(model3)
model3a<-lm(lgdp~t+tsq+s1+s3)
summary(model3a)
normtest(residuals(model3a))
plot(c(1,66),range(lgdp),type="n",main="Regression Model",
     xlab="Quarters",ylab="log-GDP in Crores of Rs.",font.main=1,cex.main=1.5)
lines(1:66,lgdp,lty=1)
points(1:66,lgdp)
lines(t,6.505e+00+2.514e-02*t+1.239e-04*tsq+4.193e-02*s1-2.645e-02*s3,col=2)
boxplot(residuals(model2))
plot(model2)
Box.test(model3a$residuals,lag=20)
Box.test(model3a$residuals,lag=20,type='L')

#time series
plot(c(1,66),range(c(lgdp)),type="n",main="log of Quarterly GDP of Trade, Hotels, Transport & Communication sector of India from 1996-97 Q1 to 2012-13 Q2",xlab="Quarters",ylab="log(GDP)")
lines(t,lgdp) 
points(t,lgdp,pch=20,col=2)
dlgdp<-diff(lgdp)
dgdptr<-diff(gdptr)
sdgdptr<-diff(gdptr,lag=4)
plot(sdgdptr,ylab="Seasonal Difference",main="Time Series Plot of Seasonal First Difference")

library(tseries)
adf.test(sdgdptr)
pp.test(sdgdptr)
kpss.test(sdgdptr)
adf.test(q4)$p.value
pp.test(q4)$p.value
kpss.test(q4)$p.value

plot(diff(diff(gdptr,lag=4)),ylab="First Difference (Seasonal+Usual)",main="Time Series Plot of the Differenced first seasonal differenced series")
adf.test(diff(diff(gdptr,lag=4)))$p.value
pp.test(diff(diff(gdptr,lag=4)))$p.value
kpss.test(diff(diff(gdptr,lag=4)))$p.value
acf((diff(gdptr,lag=4)))
pacf((diff(gdptr,lag=4)))
spectrum((diff(gdptr,lag=4)),spans=c(5,5))
stdgdp<-diff(diff(gdptr,lag=4))
acf(diff(diff(gdptr,lag=4)))
pacf(diff(diff(gdptr,lag=4)))
spectrum(diff(diff(gdptr,lag=4)),spans=c(5,5))

sarima_aics<-function(x,P,D,Q,p,d,q,n)
{
  k=1
  aic<-data.frame(matrix(ncol=7,nrow=0))
  colnames(aic)<-c("AIC","P","D","Q","p","d","q")
  for(ip in 0:p)
    for(iq in 0:q)
      for(iP in 0:P)
        for(iQ in 0:Q){
          mdl<-arima(x,order=c(iP,D,iQ),seasonal=list(order=c(ip,d,iq),period=n),method = "ML")
          aic[k,]<-c(mdl$aic,iP,D,iQ,ip,d,iq)
          k=k+1
        }
  return(aic)
}

saic_10=sarima_aics(gdptr,7,1,4,2,1,1,4)
saic_10
sort(saic_10$AIC)[1:10]
model1<-arima(gdptr,order=c(2,1,2),seasonal=list(order=c(0,1,1),period=4),method="ML")
model1
model2<-arima(gdptr,order=c(2,1,2),seasonal=list(order=c(1,1,0),period=4),method="ML")
model2
model3<-arima(gdptr,order=c(4,1,2),seasonal=list(order=c(0,1,0),period=4),method="ML")
model3
model3a<-arima(gdptr,order=c(4,1,2),seasonal=list(order=c(0,1,0),period=4),fixed=c(NA,NA,0,NA,NA,NA))
model3a
model4<-arima(gdptr,order=c(3,1,4),seasonal=list(order=c(0,1,0),period=4),method="ML")
model4
model5<-arima(gdptr,order=c(2,1,4),seasonal=list(order=c(0,1,0),period=4),method="ML")
model5
model5a<-arima(gdptr,order=c(2,1,4),seasonal=list(order=c(0,1,0),period=4),fixed=c(NA,NA,NA,NA,0,NA))
model5a
model6<-arima(gdptr,order=c(4,1,2),seasonal=list(order=c(1,1,0),period=4),method="ML")
model6
model6a<-arima(gdptr,order=c(4,1,2),seasonal=list(order=c(1,1,0),period=4),fixed=c(NA,NA,0,NA,0,NA,0))
model6a
model7<-arima(gdptr,order=c(4,1,2),seasonal=list(order=c(0,1,1),period=4),method="ML")
model7
model7a<-arima(gdptr,order=c(4,1,2),seasonal=list(order=c(0,1,1),period=4),fixed=c(NA,NA,0,NA,NA,NA,NA))
model7a
model8<-arima(gdptr,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=4),method="ML")
model8

#test
modeltest<-arima(gdptr,order=c(7,1,0),seasonal=list(order=c(0,1,1),period=4),method="ML")
modeltest
model3a
psi<-ARMAtoMA(ar=c(0.4443,-0.860,0,-0.3485), ma=c(-0.3134 ,0.8115),lag.max=30)
irfplot(psi,"IRF of Quarterly GDP")
irfplot(cumsum(psi),"cumulative GDP IRF")
psi100<-ARMAtoMA(ar=c(0.4443,-0.860,0,-0.3485), ma=c(-0.3134 ,0.8115),lag.max=100)
gamma0<-(1+sum(psi100^2))*model3a$sigma2
gamma0
(gamma0-model3a$sigma2)/gamma0 
actual_values=c(6044.79,6432.43,6253.23,6470.18)
prediction= predict(model3a,n.ahead=4)
forecasted_values =(prediction$pred)
lower_0.95=(prediction$pred-2*prediction$se)
upper_0.95=(prediction$pred+2*prediction$se)
errors=actual_values-forecasted_values
pred_df=data.frame("actual_values"=actual_values,"forecasted_values"=forecasted_values,"lower_0.95"=lower_0.95,"upper_0.95"=upper_0.95,"errors"=errors)
pred_df
rmse<-sqrt(sum((pred_df$errors)^2)/4)
rmse
mape<-sum((pred_df$errors/pred_df$actual_values))/4
mape
#########################
  
    
res<-residuals(model1)
plot(res,main='Residual Plot',ylab='Residuals')
qqnorm(res)
acf(res,ylim=c(-0.5,0.5))
pacf(res,ylim=c(-0.5,0.5))
tsdiag(model1)
normtest(res)
#White noise test in residuals
Box.test(res,lag=20)
Box.test(res,lag=20,type='L')
spectrum(res,spans=c(5,5))
spectrum(res)
spectrum(res,spans=c(3,3))
spectrum(res,spans=c(5,5))
u<-cumsum(spectrum(res)$spec)/sum(spectrum(res)$spec)
ks.test(u,"punif")
model1
#IRF
irfplot<-function (irf, s)
{
  n <- length(irf)
  plot(c(0,n+1), range(c(irf,1)), type = "n",xlab = "Time", ylab = "IRF", main = s)
  lines(c(0,n+1),c(0,0))
  lines(c(0,0),c(0,1))
  for (i in 1:n)
    lines(c(i,i),c(0, irf[i]))
}
psi<-ARMAtoMA(ar=c(0.8831,-0.9501), ma=c(-0.7107,0.8471),lag.max=30)
irfplot(psi,"IRF of Quarterly GDP")
irfplot(cumsum(psi),"cumulative GDP IRF")
psi100<-ARMAtoMA(ar=c(0.8831,-0.9501), ma=c(-0.7107,0.8471),lag.max=100)
gamma0<-(1+sum(psi100^2))*model1$sigma2
gamma0
(gamma0-model1$sigma2)/gamma0

pc2<-ARMAacf(ar=c(0.8831,-0.9501), ma=c(-0.7107,0.8471),lag.max=100,pacf=T)^2
v<-gamma0
for(i in 2:101) v[i]<-v[i-1]*(1-pc2[i-1])
((1-v/gamma0)*100)[-1]
pc2*100
cumsum(pc2*100)
((gamma0-model1$sigma2)/gamma0)*100

model1
actual_values=c(6044.79,6432.43,6253.23,6470.18)
prediction= predict(model1,n.ahead=4)
forecasted_values =(prediction$pred)
lower_0.95=(prediction$pred-2*prediction$se)
upper_0.95=(prediction$pred+2*prediction$se)
errors=actual_values-forecasted_values
pred_df=data.frame("actual_values"=actual_values,"forecasted_values"=forecasted_values,"lower_0.95"=lower_0.95,"upper_0.95"=upper_0.95,"errors"=errors)
pred_df
rmse<-sqrt(sum((pred_df$errors)^2)/4)
rmse
mape<-sum((abs(pred_df$errors)/pred_df$actual_values))/4
mape


#prediction using regression model
model3a_regression<-lm(lgdp~t+tsq+s1+s3)
summary(model3a_regression)
dq1<-c(rep(c(1,0,0,0),9),1,0)
dq2<-c(rep(c(0,1,0,0),9),0,1)
dq3<-c(rep(c(0,0,1,0),9),0,0)
t<-1:66
model3a_regression
res1<-residuals(model3a_regression)
plot(res1,main="Regression residual plot")

actual_values=c(6044.79,6432.43,6253.23,6470.18)
prediction=  predict(model3a_regression,data.frame(t=1:4,dq1=c(0,0,1,0),dq2=c(0,0,0,1),dq3=c(1,0,0,0)),interval="predict")
predict(model3a_regression,data.frame(t=67:70,dq1=c(0,0,1,0),dq2=c(0,0,0,1),dq3=c(1,0,0,0)),interval="predict") 
forecasted_values =exp(prediction$pred)
lower_0.95=(prediction$pred-2*prediction$se)
upper_0.95=(prediction$pred+2*prediction$se)
errors=actual_values-forecasted_values
pred_df=data.frame("actual_values"=actual_values,"forecasted_values"=forecasted_values,"lower_0.95"=lower_0.95,"upper_0.95"=upper_0.95,"errors"=errors)
pred_df
rmse<-sqrt(sum((pred_df$errors)^2)/4)
rmse
mape<-sum((abs(pred_df$errors)/pred_df$actual_values))/4
mape

#check
finmodel<-arima (gdptrans,order=c(7,1,0),seasonal=list(order=c(0,1,1),period=4), fixed=c(0,0,0,0,0,NA,0,NA)) 
res2<-residuals(model1_reg)
plot(res2)
#########################last part
dq1<-c(rep(c(1,0,0,0),16),1,0)
dq2<-c(rep(c(0,1,0,0),16),0,1)
dq3<-c(rep(c(0,0,1,0),16),0,0)
t<-1:66
model_reg<-lm(gdptr~t+dq1+dq2+dq3)
summary(model_reg)
AIC(model_reg)
res_reg<-residuals(model3)
library(ggplot2)
plot(lm(model3,which=c(1,2)))
plot(ts(res_reg),main="Regression residual plot")
acf(ts(res_reg),main="ACF of Regression residual")
pacf(ts(res_reg),main="PACF of Regression residual")
dq1
predict(model2,data.frame(t=67:70,dq1=c(0,0,1,0),dq2=c(0,0,0,1),dq3=c(1,0,0,0)),interval="predict")
#Predicting next 4 values using regression
AIC(model3)
M1<-matrix(c(0,-1,-1,1,1,0,0,-1,1,-1,1,0),nrow=4,ncol=3)
M2<-as.data.frame(cbind(c(67,68,69,70),M1))
colnames(M2)<-c("t","s1","s2","s3")
M2$tsq<-c(67^2,68^2,69^2,70^2)
pred_regression<-predict(model3,M2)
forecasted_values =exp(pred_regression$pred)
pred_regression<-predict(model3,M2,interval="confidence", level=.95)
pred_regression
