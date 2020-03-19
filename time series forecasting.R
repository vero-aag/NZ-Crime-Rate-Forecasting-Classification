data<-read.csv("mvic.csv") #total victims per month (2014-2019)
str(data)
myts <- ts(data[,2], start=c(2014, 7), frequency=12) 
str(myts)
myts

library(forecast)
install.packages("caret")
library(caret)
#timeseries plots - crime rate by year
autoplot(myts,ylab="victims")
plot(myts,type="o",pch=20,xlab= "Year", ylab="Victims")
library(ggplot2)

#DECOMPOSITION OF ADDITIVE TIME SERIES
myts.decomp<-decompose(myts)
autoplot(myts.decomp)
#--------------------

#best transformation
BoxCox.lambda(myts)
#1.210218
#this lambda indicates that a transformation it's not necessary

#FORECASTING with just auto.arima and ets
auto.arima(myts) #(1,0,1)(1,1,0) |No diff + seasonal diff
ets(myts) #(M,N,A) | Multiplicative Errors, NO-trend, Additive season

#all these indicates that the time series has some degree of seasonality
#and it may need a differencing
#just like the decomposition shows

autoplot(myts)
ggAcf(myts)

autoplot(log(myts))
ggAcf(log(myts))

autoplot(diff(myts))
ggAcf(diff(myts))

difflogmyts<-diff(log(myts),lag=12)
autoplot(difflogmyts)
ggAcf(difflogmyts)

Box.test(myts,type="Lj")
Box.test(log(myts),type="Lj")
Box.test(diff(myts),type="Lj")
Box.test(difflogmyts,type="Lj")     #p-value closest to 0.05    
#THE DATA BEHAVES BETTER (ACF PLOT) WHEN APPLYING ONE DIFF AND A LOG TRANSFORMATION
#--------

#the data needs one differencing and one log-transformation
#since auto.arima is going to be used, the lambda parameter will be changed only

#test and training sets

training<-window(myts,end=c(2018,6))
test<-window(myts,start=c(2018,7))

#ARIMA MODELS

fitA <- auto.arima(training,lambda=0) 
#ARIMA (0,1,2)(1,1,0) | 1 differencing each
checkresiduals(fitA) 
#p-value=0.28 - no dependence 
#ACF plot seems to be okay as well as the residuals, excepet for an outlier
summary(fitA) 
#AICc=-143.44 |RMSE=255.46 | MAPE=1.628 
fitA %>% forecast(h = 12) %>% autoplot()+autolayer(test, series="Test data")

fitA1 <- auto.arima(training,lambda=1.210218) 
#ARIMA (0,1,2)(1,1,0) | 1 differencing each
checkresiduals(fitA1) 
#p-value=0.14 - no dependence , no auto-corr
#ACF plot seems to be okay as well as the residuals, excepet for an outlier
summary(fitA1) 
#AICc=648.58 |RMSE=248.07 | MAPE=1.53 
fitA1 %>% forecast(h = 12) %>% autoplot()+autolayer(test, series="Test data")

fitA2 <- auto.arima(training) 
#ARIMA (0,1,2)(1,1,0) | 1 differencing each
checkresiduals(fitA2) 
#p-value=0.15 - no dependence , no auto-corr
#ACF plot seems to be okay as well as the residuals, excepet for an outlier
summary(fitA2) 
#AICc=510.97 |RMSE=248.14 | MAPE=1.53 
accuracy(forecast_fitA2,test)
fitA2 %>% forecast(h = 12) %>% autoplot(PI=TRUE)+autolayer(test, series="Test data")
print(fitA2)

fit.testA2<-Arima(test,model=fitA2)

forecast_fitA2<-forecast(fitA2, h=12)
autoplot(forfitA2)+autolayer(test, series="Test data")

#---------------------------

#ETS MODELS....in general, they don't need a transformation for seasonality

fitE <- ets(training, lambda=0) #(A,A,A) - Additive trend appeared
checkresiduals(fitE) 
#p-value < 0.05 | not quite close to be white nose but the ACF plot seems correct as well as the residuals distribution
summary(fitE)
#AICc=-139.5325 | RMSE=222.93 | MAPE = 1.567
fitE %>% forecast(h = 12) %>% autoplot()+autolayer(test, series="Test data")

fitE1 <- ets(training, lambda=1.210218) 
#(A,N.A)
checkresiduals(fitE1) 
#p-value < 0.05 | not quite close to be white nose but the ACF plot seems correct as well as the residuals distribution
summary(fitE1)
#AICc=938.1611 | RMSE=221.88 | MAPE = 1.552
fitE1 %>% forecast(h = 12) %>% autoplot()+autolayer(test, series="Test data")

fitE2 <- ets(training) 
#(M,N.A)
checkresiduals(fitE2) 
#p-value < 0.05 | not quite close to be white nose but the ACF plot seems correct NOT SO MUCH the normal distribution
summary(fitE2)
#AICc=748.011 | RMSE=220.80 | MAPE = 1.51
fitE2 %>% forecast(h = 12) %>% autoplot()+autolayer(test, series="Test data")

#-------------------------------

#NEURAL NETWORK MODEL

fitNN <- nnetar(training) #(2,1,2) - 1 differencing
checkresiduals(fitNN) 
Box.test(fitNN$residuals,type="Lj")
#p-value =0.81 | 
#ACF plot seems correct as well as the residuals distribution
summary(fitNN)
print(fitNN)
fitNN %>% forecast(h = 12) %>% autoplot()+autolayer(test, series="Test data")
forecast_fitNN<-forecast(fitNN, h=12, PI=TRUE)

library(forecast)
autoplot(forecast_fitNN)+autolayer(test, series="Test data")
accuracy(forecast_fitNN,test)


#-------------

#FACEBOOK'S PROPHET
data1<-read.csv("mvic.csv")
library(anytime)
data1$Date<-anydate(data$Date)
str(data1)
install.packages("ggfortify",dependencies = TRUE)
install.packages("ggplot2",dependencies = TRUE)

library(ggfortify)
library(ggplot2)
plot(data1)
summary(data1)

train1<-subset(data1,Date<"2018-07-01")
str(train1)
test1<-subset(data1,Date>"2018-06-01")

ds<-train1$Date
y<-train1$Victims
df<-data.frame(ds,y)
install.packages("prophet")
library(prophet)
m1 <- prophet(df)
future1<-make_future_dataframe(m1, periods=12, freq='month')
forecast1 <- predict(m1,future1)
str(m1)

plot(m1,forecast1, uncertainty=TRUE,xlab="Year",ylab="victims") 
lines(test1)
str(m1)
summary(forecast1)
str(forecast1)
summary(forecast1$ds)
summary(forecast1$yhat)

#ALL graphs in english:
#Sys.setlocale(category = "LC_ALL", locale = "english")#


prophet_plot_components(m1,forecast1)
library(ggplot2)

p <- ggplot()
p <- p + geom_point(data = train1, aes(x = Date, y = Victims), size = 1.5)
p
p <- p + geom_line(data = forecast1, aes(x = as.Date(ds), y = yhat), color = "#0072B2")
p
p <- p + geom_ribbon(data = forecast1, aes(x = as.Date(ds), ymin = yhat_lower, ymax = yhat_upper), fill = "#0072B2", alpha = 0.3)
p
p <- p + geom_line(data = test1, aes(x = Date, y = Victims, color='Test'), size = 0.5) + labs(color='Series')
p
str(test1)

Observations<-data1$Victims
Forecast<-forecast1$yhat
data2<-data.frame(Observations,Forecast)
data2$residuals<-data2$Observations - data2$Forecast
acf(data2$residuals)
hist(data2$residuals)

Box.test(data2$residuals,type="Lj")


#----
obs<-data1$Victims
pred<-forecast1$yhat
data3<-data.frame(obs,pred)
data3$residuals<-data3$obs - data3$pred

error.training<-data3[1:48,]
error.test<-data3[48:60,]
str(error.training)

Box.test(error.training$residuals,type="Lj")
library(fpp2)
ggAcf(error.training$residuals)
gghistogram(error.training$residuals, add.normal = TRUE) 
hist(error.training$residuals)

install.packages("Metrics")
library(Metrics)
mae(error.training$obs,error.training$pred)
mae(error.test$obs,error.test$pred)

rmse(error.training$obs,error.training$pred)
rmse(error.test$obs,error.test$pred)

mape(error.training$obs,error.training$pred)
mape(error.test$obs,error.test$pred)



