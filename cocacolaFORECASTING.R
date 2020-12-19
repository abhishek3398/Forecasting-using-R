library(forecast)
library(fpp)
library(smooth)
library(tseries)
library(moments)
library(dummies)
windows()
library(readxl)
CocaCola_Sales_Rawdata <- read_excel(choose.files())
View(CocaCola_Sales_Rawdata)

summary(CocaCola_Sales_Rawdata)
str(CocaCola_Sales_Rawdata)
skewness(CocaCola_Sales_Rawdata$Sales)
kurtosis(CocaCola_Sales_Rawdata$Sales)

barplot(CocaCola_Sales_Rawdata$Sales)
boxplot(CocaCola_Sales_Rawdata$Sales)
hist(CocaCola_Sales_Rawdata$Sales)
plot(CocaCola_Sales_Rawdata$Sales, type = "o")

Q1 <-  ifelse(grepl("Q1",CocaCola_Sales_Rawdata$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",CocaCola_Sales_Rawdata$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",CocaCola_Sales_Rawdata$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",CocaCola_Sales_Rawdata$Quarter),'1','0')

cocacola <-cbind(CocaCola_Sales_Rawdata,Q1,Q2,Q3,Q4)
cocacola["t"]<- 1:42
cocacola["t^2"] <- cocacola["t"]*cocacola["t"]
cocacola["log(sales)"] <- log(cocacola["Sales"])
View(cocacola)
colnames(cocacola)
class(cocacola)

attach(cocacola)

pd <- sample(2, NROW(cocacola), replace = TRUE, prob = c(0.8,0.2))
train <- cocacola[pd==1,]
test <- cocacola[pd==2,]

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear

expo_model<-lm(`log(sales)`~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo 

Quad_model<-lm(Sales~t+t^2,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear

Add_sea_Quad_model<-lm(Sales~t+t^2+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

multi_sea_model<-lm(`log(sales)`~ Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

multi_add_sea_model<-lm(`log(sales)`~t+Q1+Q2+Q3+Q4, data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multi Additive seasonality has least RMSE value #

new_model <- lm(`log(sales)`~t+Q1+Q2+Q3+Q4,data = cocacola)

resid <- residuals(new_model)
acf(resid,lag.max = 12)

k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))
acf(k$residuals,lag.max = 12)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(k$residuals)

