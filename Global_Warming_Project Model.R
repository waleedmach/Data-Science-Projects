#Q1####
#NASA####
#read data
library(forecast)
library(TSA)
library(tidyverse)
NASA<-read.csv(file.choose(), header=TRUE, sep=",", skip=1) 
tidynasa <- NASA[-c(14:19)] 
tidynasa <- gather(tidynasa, Month, Temp, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ) # transpose the data
tidynasa$Month <- match(tidynasa$Month, month.abb) # convert months to numbers
tidynasa <- tidynasa[with(tidynasa, order(Year, Month)), ]  #order by month
names(tidynasa) <- c("Year", "Month", "Temperature") 
rownames(tidynasa) <- 1:nrow(tidynasa) #update the row names

GLB <- tidynasa
GLB$Temperature <- as.numeric(GLB$Temperature ) + 14
GLB2 <- head(GLB, -9)
str(GLB)
#One - ts models####

#Defines the dataset as timeseries starting Jan 1880 and having seasonality of frequency 12 (monthly)
GLB_ts <- ts(GLB2$Temperature,start=1880, frequency=12) 
GLB_ts
plot(GLB_ts)
#compare this year and last year temp.
plot(y=GLB_ts, x=zlag(GLB_ts), ylab="Temp.", xlab = 'last year Temp.')
thisyear <- GLB_ts
lastyear <- zlag(GLB_ts)
cor(thisyear[2:1695],lastyear[2:1695])
#plot various decomposition into error/noise, trend and seasonality
fit <- stl(GLB_ts, t.window=12, s.window="periodic") #decompose using STL (Season and trend using Loess)
plot(fit)

#1.1 - ets models (total 5)####
GLB_AAN <- ets(GLB_ts, model="AAN")
GLB_AAZ <- ets(GLB_ts, model="AAZ", damped=FALSE)
GLB_AAA <- ets(GLB_ts, model="AAA", damped=FALSE)
GLB_MMZ <- ets(GLB_ts, model="MMZ", damped=FALSE)
GLB_MMN <- ets(GLB_ts, model="MMN", damped=FALSE)
GLB_MMM <- ets(GLB_ts, model="MMM", damped=FALSE)
GLB_MAM <- ets(GLB_ts, model="MAM", damped=FALSE)
GLB_MNN <- ets(GLB_ts)
summary(GLB_MNN)
#Create their prediction "cones" for 972 months (79 years to 2100 year) into the future with quintile confidence intervals 79 years, 90% confidence interval and 95% confidence interval
GLB_AAN_pred <- forecast(GLB_AAN, h=972, level=c(0.9, 0.95)) 
GLB_AAZ_pred <- forecast(GLB_AAZ, h=972, level=c(0.9, 0.95))
GLB_AAA_pred <- forecast(GLB_AAA, h=972, level=c(0.9, 0.95))
GLB_MMZ_pred <- forecast(GLB_MMZ, h=972, level=c(0.9, 0.95))
GLB_MMN_pred <- forecast(GLB_MMN, h=972, level=c(0.9, 0.95))
GLB_MMM_pred <- forecast(GLB_MMM, h=972, level=c(0.9, 0.95))
GLB_MAM_pred <- forecast(GLB_MAM, h=972, level=c(0.9, 0.95))
GLB_MNN_pred <- forecast(GLB_MNN, h=972, level=c(0.9, 0.95))
# Compare the prediction "cones" visually
par(mfrow=c(1,1)) 
plot(GLB_AAN_pred, xlab="Year", ylab="Predicted Temp.")
plot(GLB_AAZ_pred, xlab="Year", ylab="Predicted Temp.") #AAN, seasonality change to automatic results in "N" for this data
plot(GLB_AAA_pred, xlab="Year", ylab="Predicted Temp.")
plot(GLB_MMZ_pred, xlab="Year", ylab="Predicted Temp.") #MMN, seasonality change to automatic results in "N" for this data
plot(GLB_MMN_pred, xlab="Year", ylab="Predicted Temp.")
plot(GLB_MMM_pred, xlab="Year", ylab="Predicted Temp.")
plot(GLB_MAM_pred, xlab="Year", ylab="Predicted Temp.")
plot(GLB_MNN_pred, xlab="Year", ylab="Predicted Temp.")
# ESTs:Total 5 est models - Lets look at what our models actually are -- ETS
GLB_AAN
GLB_AAA
GLB_MMN
GLB_MMM
GLB_MAM
GLB_MNN


#1.2 - TBATS Model####
GLB_tbats <- tbats(GLB_ts)
GLB_tbats_pred <-forecast(GLB_tbats, h=972, level=c(0.9, 0.95))
plot(GLB_tbats_pred, xlab="Year", ylab="Predicted Temp.", ylim=c(10,20))# does not capture the trend


#1.3 - Plain ARIMA####
# auto-correlation function
Acf(GLB_ts,main="") 
pacf(GLB_ts,main="")
#We could see multi seasonality inside the data, so we will try mts later
Acf(log(GLB_ts),main="") # log-transformed data
Pacf(log(GLB_ts),main="")

Acf(diff(GLB_ts,10),main="")
Pacf(diff(GLB_ts,10),main="")

Acf(diff(log(GLB_ts),10),main="") # difference-12 log data
Pacf(diff(log(GLB_ts),10),main="") 

fit_ARIMA <- auto.arima(GLB_ts,seasonal=TRUE)

Acf(residuals(fit_ARIMA))
Pacf(residuals(fit_ARIMA))

plot(forecast(fit_ARIMA,972))
GLB_ARIMA_pred <-  forecast(fit_ARIMA,972)


#1.4 - Comparing models####
f_AAN  <- function(y, h) forecast(ets(y, model="AAN"), h = h)
errors_AAN <- tsCV(GLB_ts, f_AAN, h=1, window=848)

f_AAA  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
errors_AAA <- tsCV(GLB_ts, f_AAA, h=1, window=848)

f_MMN  <- function(y, h) forecast(ets(y, model="MMN"), h = h)
errors_MMN <- tsCV(GLB_ts, f_MMN, h=1, window=848)

f_MMM  <- function(y, h) forecast(ets(y, model="MMM"), h = h)
errors_MMM <- tsCV(GLB_ts, f_MMM, h=1, window=848)

f_MAM  <- function(y, h) forecast(ets(y, model="MAM"), h = h)
errors_MAM <- tsCV(GLB_ts, f_MAM, h=1, window=848)

f_ANN  <- function(y, h) forecast(ets(y, model="MNN"), h = h)
errors_ANN <- tsCV(GLB_ts, f_ANN, h=1, window=848)

mean(abs(errors_AAN/GLB_ts), na.rm=TRUE)*100 #MAPE is 0.6104427
mean(abs(errors_AAA/GLB_ts), na.rm=TRUE)*100 #MAPE is 0.6141765
mean(abs(errors_MMN/GLB_ts), na.rm=TRUE)*100 #MAPE is 0.6098554
mean(abs(errors_MMM/GLB_ts), na.rm=TRUE)*100 #MAPE is 0.6148312
mean(abs(errors_MAM/GLB_ts), na.rm=TRUE)*100 #MAPE is 0.6130813
mean(abs(errors_ANN/GLB_ts), na.rm=TRUE)*100 #MAPE is 0.6097794

plot(GLB_ARIMA_pred$residuals)
acf(GLB_ARIMA_pred$residuals)


#Two - mts models####
CLB3 <- GLB2
newcol <- rep(c(rep(1,times =12),rep(2, times = 12) , rep(3, times =12),
                rep(4,times = 12), rep(5, times = 12), rep(6,times =12),
                rep(7,times = 12),rep(8,times=12), rep(9,times = 12) ,
                rep(10,times = 12)),times=17)


newcol2<-newcol[1:1695]
CLB3$Yearin10 <- newcol2
str(CLB3)
CLB3$Yearin10 <- as.factor(CLB3$Yearin10)
CLB3$X <- as.factor(CLB3$X)
str(CLB3)
summary(CLB3) 
GLB_msts <- msts (CLB3$Temperature, seasonal.periods=c(12,120)) 


#2.1 - TBATS####
GLB_msts_tbats <- tbats(GLB_msts)
plot(GLB_msts_tbats) #plot decomposition
GLB_msts_tbats_pred <- forecast(GLB_msts_tbats, h=972, level=c(0.9, 0.95)) #predict 2 weeks out
plot(GLB_msts_tbats_pred, xlab="Time", ylab="Predicted Tepm.")


#2.21 - plain ARIMA####
GLB_msts_arima <- auto.arima(GLB_msts,seasonal=TRUE)
GLB_msts_pred <- forecast(GLB_msts_arima, h=972, level=c(0.9, 0.95))
plot(GLB_msts_pred, xlab="Time", ylab="Predicted Temp.")

#2.22 - USETHIS- Auto Arima(3,1,1)(0,1,0)####
GLB_msts_arima2 <- auto.arima(GLB_msts, seasonal = TRUE, trace = TRUE , D=1)
GLB_msts_arima2_pred <- forecast(GLB_msts_arima2, h=972, level=c(0.8,0.90))
plot(GLB_msts_arima2_pred)

#2.3 - ARIMA with regressors####
TenYearMatrix <- cbind(Yearin10=model.matrix(~as.factor(CLB3$Yearin10)))
TenYearMatrix <- TenYearMatrix[,-1]
colnames(TenYearMatrix) <- c("1_10years","2_10years","3_10years","4_10years","5_10years","6_10years",
                             "7_10years","8_10years","9_10years") 

matrix_of_regressors <- TenYearMatrix

GLB_msts_arima <- auto.arima(CLB3$Temperature, xreg=matrix_of_regressors) # Train a model 
GLB_msts_arima # See what it is

xreg.pred<-matrix_of_regressors[-c(973:1695),] 
GLB_msts_arima_pred <- forecast(GLB_msts_arima, h=972, xreg = xreg.pred, level=c(0.9, 0.95))
plot(GLB_msts_arima_pred, xlab="Time", ylab="Predicted Temp.", ylim=c(13,17))



#2.4 - ARIMA on residuals####
GLB_msts_lm_msts <- tslm(GLB_msts ~ trend + season) # Build a linear model for trend and seasonality
summary(GLB_msts_lm_msts)

residarima1 <- auto.arima(GLB_msts_lm_msts$residuals) # Build ARIMA on it's residuals
residarima1
residualsArimaForecast <- forecast(residarima1, h=972) #forecast from ARIMA
residualsF <- as.numeric(residualsArimaForecast$mean)

regressionForecast <- forecast(GLB_msts_lm_msts,h=972) #forecast from lm
regressionF <- as.numeric(regressionForecast$mean)

forecastR <- regressionF+residualsF # Total prediction
print(forecastR)
for (i in 1:972){points(i+1696,forecastR[i],col="red",pch=19, cex=0.5)}

#compare with TBATS
plot(GLB_msts_tbats_pred, xlab="Time", ylab="Predicted Tep.")
for (i in 1:972){points((i+1696+120)/(10*12),forecastR[i],col="red",pch=19, cex=0.5)}



#Three - Rolling-horizon holdout####
#3.1 - TBATS####
accuracy.tbats=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:10)
{ 
  nTest <- 12*i  
  nTrain <- length(GLB_msts)- nTest - 1
  train <- window(GLB_msts, start=1, end=1+(nTrain)/(120))
  test <- window(GLB_msts, start=1+(nTrain+1)/(120), end=1+(nTrain+12)/(120))
  
  s <- tbats(train)
  sp<- predict(s,h=12)
  
  print(accuracy(sp,test))
  
  accuracy.tbats<-rbind(accuracy.tbats,accuracy(sp,test)[2,5])
  
}
accuracy.tbats<-accuracy.tbats[-1] 


#3.2 - ARIMA on residuals####
accuracy.arima=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:10)
{
  nTest <- 12*i
  nTrain <- length(GLB_msts)- nTest -1
  train <- window(GLB_msts, start=1, end=1+(nTrain)/(120))
  test <- window(GLB_msts, start=1+(nTrain+1)/(120), end=1+(nTrain+12)/(120))
  trainlm <- tslm(train ~ trend + season)
  trainlmf <- forecast(trainlm,h=12)
  residauto <- auto.arima(trainlm$residuals)
  residf <- forecast(residauto,h=12)
  y <- as.numeric(trainlmf$mean)
  x <- as.numeric(residf$mean)
  sp <- x+y
  cat("----------------------------------
      Data Partition",i,"
      Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
      Test Set includes 12 time periods. Observations", nTrain+1, "to", nTrain+12,"
      ")
  print(accuracy(sp,test))
  #  print(residauto)
  accuracy.arima<-rbind(accuracy.arima,accuracy(sp,test)[1,5])
  #print(sp$model)
}

accuracy.arima<-accuracy.arima[-1]

#3.3 - validation on auto ARIMA(3,1,1)####
nasa_train <- subset(CLB3, Year <= 2000) # split train and test data by 2000 
nasa_test <- subset(CLB3, Year > 2000)
nasa_msts_train <- msts(nasa_train$Temperature, seasonal.periods = c(12,120))
nasa_arima2_train <- auto.arima(nasa_msts_train, seasonal = TRUE, trace = TRUE, D = 1)
nasa_arima2_train_pridect <- forecast(nasa_arima2_train, h=243, level=c(0.9,0.95))
plot(nasa_arima2_train_pridect, xlab = 'Year', ylab = "Predicted Temp.")

nasa_arima_pred2_date <- data.frame("Date" =seq(as.Date("2001/1/1"), by = "month", length.out =243))
nasa_arima_pred2_train_final <- cbind(nasa_arima_pred2_date,nasa_arima2_train_pridect)
nasa_arima_pred2_final_data <- data.frame("Date" =nasa_arima_pred2_train_final$Date,"forecast temp." = nasa_arima_pred2_train_final$`Point Forecast`) 
nasa_arima_pred2_final_data$ActualTemp <- nasa_test$Temperature
mape_nasa_autoarima_pred2 <- MLmetrics::MAPE(nasa_arima_pred2_final_data$forecast.temp.,nasa_arima_pred2_final_data$ActualTemp) #0.02158156



#3.3 - validation ARIMA on regressor####
nasa_train <- subset(CLB3, Year <= 2000) # split train and test data by 2000 
nasa_test <- subset(CLB3, Year > 2000)

TenYearMatrix_train <- cbind(Yearin10=model.matrix(~as.factor(nasa_train$Yearin10)))
TenYearMatrix_train <- TenYearMatrix_train[,-1]
colnames(TenYearMatrix_train) <- c("1_10years","2_10years","3_10years","4_10years","5_10years","6_10years",
                             "7_10years","8_10years","9_10years") 

matrix_of_train_regressors <- TenYearMatrix_train

GLB_msts_arima <- auto.arima(nasa_train$Temperature, xreg=matrix_of_train_regressors)  
GLB_msts_arima # See what it is

xreg_pred<-matrix_of_train_regressors[-c(244:1452),] 
GLB_test_msts_arima_pred <- forecast(GLB_msts_arima, h=243, xreg = xreg_pred, level=c(0.9, 0.95))
plot(GLB_test_msts_arima_pred, xlab="Time", ylab="Predicted Temp.", ylim=c(13,17)) #start from 1453 rows

nasa_reg_pred_date <- data.frame("Date" =seq(as.Date("2001/1/1"), by = "month", length.out =243))
nasa_reg_pred2_train_final <- cbind(nasa_reg_pred_date,GLB_test_msts_arima_pred)
nasa_reg_pred2_final_data <- data.frame("Date" =nasa_reg_pred2_train_final$Date,"forecast temp." = nasa_reg_pred2_train_final$`Point Forecast`) 
nasa_reg_pred2_final_data$ActualTemp <- nasa_test$Temperature
mape_nasa_reg_pred2 <- MLmetrics::MAPE(nasa_reg_pred2_final_data$forecast.temp.,nasa_reg_pred2_final_data$ActualTemp) #0.01922061

#3.4 - Results - compare mean accuracies of the rolling holdout####
MAPE_mean_tbats <- mean(accuracy.tbats) #0.9048769
MAPE_mean_arima <- mean(accuracy.arima) #0.9571464
MAPE_std_tbats <- sd(accuracy.tbats) #0.3137239
MAPE_std_arima <- sd(accuracy.arima) #0.3721376
mape_nasa_autoarima_pred2 #0.02158156
mape_nasa_reg_pred2 #0.01922061

#Export file####
#4.1 - Export the NASA auto ARIMA mts model prediction####
GLB_nasa_date <- data.frame("Date" =seq(as.Date("2021/4/1"), by = "month", length.out =972))
nasa_prediction <- cbind(GLB_nasa_date,GLB_msts_arima2_pred)
write.csv(nasa_prediction, "nasa_prediction.csv", row.names = FALSE)


#MET####
#read data
library(forecast)
library(TSA)
library("tidyverse")

Hadcrut4<-read.csv(file.choose(), header=TRUE, sep=",") 
UKMetData <- subset(Hadcrut4, select = c(Date,Median))
UKMetData <- UKMetData %>% separate(Date,c("Year","Month"))
UKMetData$Year <- as.numeric(UKMetData$Year)
UKMetData$Month<- as.numeric(UKMetData$Month)
rownames(UKMetData) <- 1:nrow(UKMetData) #update the row names
names(UKMetData) <- c("Year","Month","Temperature") # update the names of the columns
write.csv(UKMetData, "Hadcrut4_Processed.csv", row.names = FALSE)

MET <- UKMetData
str(MET)
MET$Temperature <- MET$Temperature  + 14
#One - ts models####

#Defines the dataset as timeseries starting Jan 1880 and having seasonality of frequency 12 (monthly)
MET_ts <- ts(MET$Temperature,start=1850, frequency=12) 
MET_ts
plot(MET_ts)
#compare this year and last year temp.
plot(y=MET_ts, x=zlag(MET_ts), ylab="Temp.", xlab = 'last year Temp.')
thisyear <- MET_ts
lastyear <- zlag(MET_ts)
cor(thisyear[2:2053],lastyear[2:2053])
#plot various decomposition into error/noise, trend and seasonality
fit <- stl(MET_ts, t.window=12, s.window="periodic") #decompose using STL (Season and trend using Loess)
plot(fit)

#1.1 - ets models (total 5)####
MET_AAN <- ets(MET_ts, model="AAN")
MET_AAZ <- ets(MET_ts, model="AAZ", damped=FALSE)
MET_AAA <- ets(MET_ts, model="AAA", damped=FALSE)
MET_MMZ <- ets(MET_ts, model="MMZ", damped=FALSE)
MET_MMN <- ets(MET_ts, model="MMN", damped=FALSE)
MET_MMM <- ets(MET_ts, model="MMM", damped=FALSE)
MET_MAM <- ets(MET_ts, model="MAM", damped=FALSE)
MET_MNN <- ets(MET_ts)
summary(MET_MNN)
#Create their prediction "cones" for 959 months (79 years to 2100 year) into the future with quintile confidence intervals 79 years, 90% confidence interval and 95% confidence interval
MET_AAN_pred <- forecast(MET_AAN, h=959, level=c(0.9, 0.95)) 
MET_AAZ_pred <- forecast(MET_AAZ, h=959, level=c(0.9, 0.95))
MET_AAA_pred <- forecast(MET_AAA, h=959, level=c(0.9, 0.95))
MET_MMZ_pred <- forecast(MET_MMZ, h=959, level=c(0.9, 0.95))
MET_MMN_pred <- forecast(MET_MMN, h=959, level=c(0.9, 0.95))
MET_MMM_pred <- forecast(MET_MMM, h=959, level=c(0.9, 0.95))
MET_MAM_pred <- forecast(MET_MAM, h=959, level=c(0.9, 0.95))
MET_MNN_pred <- forecast(MET_MNN, h=959, level=c(0.9, 0.95))
# Compare the prediction "cones" visually
par(mfrow=c(1,1)) 
plot(MET_AAN_pred, xlab="Year", ylab="Predicted Temp.")
plot(MET_AAZ_pred, xlab="Year", ylab="Predicted Temp.") #AAA, seasonality change to automatic results in "N" for this data
plot(MET_AAA_pred, xlab="Year", ylab="Predicted Temp.")
plot(MET_MMZ_pred, xlab="Year", ylab="Predicted Temp.") #MMN, seasonality change to automatic results in "N" for this data
plot(MET_MMN_pred, xlab="Year", ylab="Predicted Temp.")
plot(MET_MMM_pred, xlab="Year", ylab="Predicted Temp.")
plot(MET_MAM_pred, xlab="Year", ylab="Predicted Temp.")
plot(MET_MNN_pred, xlab="Year", ylab="Predicted Temp.")
# ESTs:Total 5 est models - Lets look at what our models actually are -- ETS
MET_AAN
MET_AAA
MET_MMN
MET_MMM
MET_MAM
MET_MNN

#1.2 - TBATS Model####
MET_tbats <- tbats(MET_ts)
MET_tbats_pred <-forecast(MET_tbats, h=959, level=c(0.9, 0.95))
plot(MET_tbats_pred, xlab="Year", ylab="Predicted Temp.", ylim=c(10,20))# does not capture the trend

par(mfrow=c(1,1)) 
#1.3 - Plain ARIMA####
# auto-correlation function
Acf(MET_ts,main="") 
pacf(MET_ts,main="")
#We could see multi seasonality inside the data, so we will try mts later
Acf(log(MET_ts),main="") # log-transformed data
Pacf(log(MET_ts),main="")

Acf(diff(MET_ts,10),main="")
Pacf(diff(MET_ts,10),main="")

Acf(diff(log(MET_ts),10),main="") # differ ence-12 log data
Pacf(diff(log(MET_ts),10),main="") 

fit_ARIMA <- auto.arima(MET_ts,seasonal=TRUE)

Acf(residuals(fit_ARIMA))
Pacf(residuals(fit_ARIMA))

plot(forecast(fit_ARIMA,959))
MET_ARIMA_pred <-  forecast(fit_ARIMA,959)


#1.4 - Comparing models####
f_AAN  <- function(y, h) forecast(ets(y, model="AAN"), h = h)
errors_AAN <- tsCV(MET_ts, f_AAN, h=1, window=1000)

f_AAA  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
errors_AAA <- tsCV(MET_ts, f_AAA, h=1, window=1000)

f_MMN  <- function(y, h) forecast(ets(y, model="MMN"), h = h)
errors_MMN <- tsCV(MET_ts, f_MMN, h=1, window=1000)

f_MMM  <- function(y, h) forecast(ets(y, model="MMM"), h = h)
errors_MMM <- tsCV(MET_ts, f_MMM, h=1, window=1000)

f_MAM  <- function(y, h) forecast(ets(y, model="MAM"), h = h)
errors_MAM <- tsCV(MET_ts, f_MAM, h=1, window=1000)

f_ANN  <- function(y, h) forecast(ets(y, model="MNN"), h = h)
errors_ANN <- tsCV(MET_ts, f_ANN, h=1, window=1000)

mean(abs(errors_AAN/MET_ts), na.rm=TRUE)*100 #MAPE is 0.5463387
mean(abs(errors_AAA/MET_ts), na.rm=TRUE)*100 #MAPE is 0.543987
mean(abs(errors_MMN/MET_ts), na.rm=TRUE)*100 #MAPE is 0.5462563
mean(abs(errors_MMM/MET_ts), na.rm=TRUE)*100 #MAPE is 0.5447151
mean(abs(errors_MAM/MET_ts), na.rm=TRUE)*100 #MAPE is 0.548876
mean(abs(errors_ANN/MET_ts), na.rm=TRUE)*100 #MAPE is 0.5463109

plot(MET_ARIMA_pred$residuals)
acf(MET_ARIMA_pred$residuals)


#Two - mts models####
MET3 <- MET
newcol <- rep(c(rep(1,times =12),rep(2, times = 12) , rep(3, times =12),
                rep(4,times = 12), rep(5, times = 12), rep(6,times =12),
                rep(7,times = 12),rep(8,times=12), rep(9,times = 12) ,
                rep(10,times = 12)),times=18)


newcol2<-newcol[1:2053]
MET3$Yearin10 <- newcol2
str(MET3)
MET3$Yearin10 <- as.factor(MET3$Yearin10)
MET3$X <- as.factor(MET3$X)
str(MET3)
summary(MET3) 
MET_msts <- msts(MET3$Temperature, seasonal.periods=c(12,120)) 


#2.1 - TBATS####
MET_msts_tbats <- tbats(MET_msts)
plot(MET_msts_tbats) #plot decomposition
MET_msts_tbats_pred <- forecast(MET_msts_tbats, h=959, level=c(0.9, 0.95)) #predict 2 weeks out
plot(MET_msts_tbats_pred, xlab="Time", ylab="Predicted Tepm.")

par(mfrow=c(1,2)) 
#2.21 - plain ARIMA####
MET_msts_arima <- auto.arima(MET_msts,seasonal=TRUE)
MET_msts_pred <- forecast(MET_msts_arima, h=959, level=c(0.9, 0.95))
plot(MET_msts_pred, xlab="Time", ylab="Predicted Temp.")

#2.22 - USETHIS- Auto Arima(3,1,1)(0,1,0)[120]####
MET_msts_arima2 <- auto.arima(MET_msts, seasonal = TRUE, trace = TRUE , D=1)
MET_msts_arima2_pred <- forecast(MET_msts_arima2, h=959, level=c(0.8,0.90))
plot(MET_msts_arima2_pred)

#2.3 - ARIMA with regressors####
TenYearMatrix <- cbind(Yearin10=model.matrix(~as.factor(MET3$Yearin10)))
TenYearMatrix <- TenYearMatrix[,-1]
colnames(TenYearMatrix) <- c("1_10years","2_10years","3_10years","4_10years","5_10years","6_10years",
                             "7_10years","8_10years","9_10years") 

matrix_of_regressors <- TenYearMatrix

MET_msts_arima <- auto.arima(MET3$Temperature, xreg=matrix_of_regressors) # Train a model 
MET_msts_arima # See what it is

xreg.pred<-matrix_of_regressors[-c(956:2053),] 
MET_msts_arima_pred <- forecast(MET_msts_arima, h=959, xreg = xreg.pred, level=c(0.9, 0.95))
plot(MET_msts_arima_pred, xlab="Time", ylab="Predicted Temp.", ylim=c(13,17))



#2.4 - ARIMA on residuals####
MET_msts_lm_msts <- tslm(MET_msts ~ trend + season) # Build a linear model for trend and seasonality
summary(MET_msts_lm_msts)

residarima1 <- auto.arima(MET_msts_lm_msts$residuals) # Build ARIMA on it's residuals
residarima1
residualsArimaForecast <- forecast(residarima1, h=959) #forecast from ARIMA
residualsF <- as.numeric(residualsArimaForecast$mean)

regressionForecast <- forecast(MET_msts_lm_msts,h=959) #forecast from lm
regressionF <- as.numeric(regressionForecast$mean)

forecastR <- regressionF+residualsF # Total prediction
print(forecastR)
for (i in 1:959){points(i+2053,forecastR[i],col="red",pch=19, cex=0.5)}


#Three - Rolling-horizon holdout####
#3.1 - TBATS####
accuracy.tbats=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:10)
{ 
  nTest <- 12*i  
  nTrain <- length(MET_msts)- nTest - 1
  train <- window(MET_msts, start=1, end=1+(nTrain)/(120))
  test <- window(MET_msts, start=1+(nTrain+1)/(120), end=1+(nTrain+12)/(120))
  
  s <- tbats(train)
  sp<- predict(s,h=12)
  
  print(accuracy(sp,test))
  
  accuracy.tbats<-rbind(accuracy.tbats,accuracy(sp,test)[2,5])
  
}
accuracy.tbats<-accuracy.tbats[-1] 


#3.2 - ARIMA on residuals####
accuracy.arima=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:10)
{
  nTest <- 12*i
  nTrain <- length(MET_msts)- nTest -1
  train <- window(MET_msts, start=1, end=1+(nTrain)/(120))
  test <- window(MET_msts, start=1+(nTrain+1)/(120), end=1+(nTrain+12)/(120))
  trainlm <- tslm(train ~ trend + season)
  trainlmf <- forecast(trainlm,h=12)
  residauto <- auto.arima(trainlm$residuals)
  residf <- forecast(residauto,h=12)
  y <- as.numeric(trainlmf$mean)
  x <- as.numeric(residf$mean)
  sp <- x+y
  cat("----------------------------------
      Data Partition",i,"
      Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
      Test Set includes 12 time periods. Observations", nTrain+1, "to", nTrain+12,"
      ")
  print(accuracy(sp,test))
  #  print(residauto)
  accuracy.arima<-rbind(accuracy.arima,accuracy(sp,test)[1,5])
  #print(sp$model)
}

accuracy.arima<-accuracy.arima[-1]

#3.3 - validation on auto ARIMA(3,1,1)(0,1,0)[120]####
MET_train <- subset(MET3, Year <= 2000) # split train and test data by 2000 
MET_test <- subset(MET3, Year > 2000)
MET_msts_train <- msts(MET_train$Temperature, seasonal.periods = c(12,120))
MET_arima2_train <- auto.arima(MET_msts_train, seasonal = TRUE, trace = TRUE, D = 1)
MET_arima2_train_pridect <- forecast(MET_arima2_train, h=241, level=c(0.9,0.95))
plot(MET_arima2_train_pridect, xlab = 'Year', ylab = "Predicted Temp.")

MET_arima_pred2_date <- data.frame("Date" =seq(as.Date("2001/1/1"), by = "month", length.out =241))
MET_arima_pred2_train_final <- cbind(MET_arima_pred2_date,MET_arima2_train_pridect)
MET_arima_pred2_final_data <- data.frame("Date" =MET_arima_pred2_train_final$Date,"forecast temp." = MET_arima_pred2_train_final$`Point Forecast`) 
MET_arima_pred2_final_data$ActualTemp <- MET_test$Temperature
mape_MET_autoarima_pred2 <- MLmetrics::MAPE(MET_arima_pred2_final_data$forecast.temp.,MET_arima_pred2_final_data$ActualTemp) #0.0121885
mape_MET_autoarima_pred2

#3.3 - validation ARIMA on regressor####
MET_train <- subset(MET3, Year <= 2000) # split train and test data by 2000 
MET_test <- subset(MET3, Year > 2000)

TenYearMatrix_train <- cbind(Yearin10=model.matrix(~as.factor(MET_train$Yearin10)))
TenYearMatrix_train <- TenYearMatrix_train[,-1]
colnames(TenYearMatrix_train) <- c("1_10years","2_10years","3_10years","4_10years","5_10years","6_10years",
                                   "7_10years","8_10years","9_10years") 

matrix_of_train_regressors <- TenYearMatrix_train

MET_msts_arima <- auto.arima(MET_train$Temperature, xreg=matrix_of_train_regressors)  
MET_msts_arima # See what it is

xreg_pred<-matrix_of_train_regressors[-c(242:2053),] 
MET_test_msts_arima_pred <- forecast(MET_msts_arima, h=241, xreg = xreg_pred, level=c(0.9, 0.95))
plot(MET_test_msts_arima_pred, xlab="Time", ylab="Predicted Temp.", ylim=c(13,17)) #start from 1453 rows

MET_reg_pred_date <- data.frame("Date" =seq(as.Date("2001/1/1"), by = "month", length.out =241))
MET_reg_pred2_train_final <- cbind(MET_reg_pred_date,MET_test_msts_arima_pred)
MET_reg_pred2_final_data <- data.frame("Date" =MET_reg_pred2_train_final$Date,"forecast temp." = MET_reg_pred2_train_final$`Point Forecast`) 
MET_reg_pred2_final_data$ActualTemp <- MET_test$Temperature
mape_MET_reg_pred2 <- MLmetrics::MAPE(MET_reg_pred2_final_data$forecast.temp.,MET_reg_pred2_final_data$ActualTemp) #0.01652815

#3.4 - Results - compare mean accuracies of the rolling holdout####
MAPE_mean_tbats <- mean(accuracy.tbats) #0.7763326
MAPE_mean_arima <- mean(accuracy.arima) #0.630832
MAPE_std_tbats <- sd(accuracy.tbats) #0.3404944
MAPE_std_arima <- sd(accuracy.arima) #0.3507041
mape_MET_autoarima_pred2 #0.0121885
mape_MET_reg_pred2 #0.01652815

#Export file####
#4.1 - Export the MET auto ARIMA mts model prediction####
MET_date <- data.frame("Date" =seq(as.Date("2021/2/1"), by = "month", length.out =959))
MET_prediction <- cbind(MET_date,MET_msts_arima2_pred)
write.csv(MET_prediction, "MET_prediction.csv", row.names = FALSE)




# Q2 ----------------------------------------------------------------------

#### NASA Prediction ####

NASA_Predict <- data.frame("Date" = nasa_prediction$Date, "point_forecast" = nasa_prediction$`Point Forecast`, "lo_90" = nasa_prediction$`Lo 90`, "hi_90" = nasa_prediction$`Hi 90`)
Jan_2030 <- NASA_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2030-01-01")
Jul_2030 <- NASA_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2030-07-01")
Jan_2050 <- NASA_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2050-01-01")
Jul_2050 <- NASA_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2050-07-01")
Jan_2100 <- NASA_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2100-01-01")
Jul_2100 <- NASA_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2100-07-01")
NASA_Predict <- rbind(Jan_2030,Jul_2030,Jan_2050,Jul_2050,Jan_2100,Jul_2100)
write.csv(NASA_Predict, "NASA Q2 Answers.csv", row.names = FALSE)


#### MET Prediction ####

MET_Predict <- data.frame("Date" = MET_prediction$Date, "point_forecast" = MET_prediction$`Point Forecast`, "lo_90" = MET_prediction$`Lo 90`, "hi_90" = MET_prediction$`Hi 90`)
Jan_2030 <- MET_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2030-01-01")
Jul_2030 <- MET_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2030-07-01")
Jan_2050 <- MET_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2050-01-01")
Jul_2050 <- MET_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2050-07-01")
Jan_2100 <- MET_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2100-01-01")
Jul_2100 <- MET_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2100-07-01")
MET_Predict <- rbind(Jan_2030,Jul_2030,Jan_2050,Jul_2050,Jan_2100,Jul_2100)
write.csv(MET_Predict, "MET Q2 Answers.csv", row.names = FALSE)




# Q3 ----------------------------------------------------------------------

kingston <- read.csv(file.choose(), header=TRUE, sep=",") 
tidykingston <- kingston[-c(14:18)] 
tidykingston <- gather(tidykingston, Month, Temp, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ) # transpose the data
tidykingston$Month <- match(tidykingston$Month, month.abb) # convert months to numbers
tidykingston <- tidykingston[with(tidykingston, order(Year, Month)), ]  #order by month
rownames(tidykingston) <- 1:nrow(tidykingston) #update the row names
tidykingston$Temp <- as.numeric(tidykingston$Temp ) + 14

write.csv(tidykingston, "Kingston Data_Processed.csv", row.names = FALSE)

kingston_ts <- ts(tidykingston$Temp, start = 1850, frequency = 12)
plot(kingston_ts)

#Compare this year with last year
plot(y=kingston_ts, x=zlag(kingston_ts), ylab="Temp.", xlab = 'last year Temp.')
thisyear <- kingston_ts
lastyear <- zlag(kingston_ts)
cor(thisyear[2:346],lastyear[2:346])

#Decompose data
plot(decompose(kingston_ts))

kingston_msts <- msts(tidykingston$Temp, seasonal.periods = c(12,120))
plot(decompose(kingston_msts))

#Auto Arima Best model: ARIMA(1,0,1)(0,1,0)[120] with drift 
kingstonarima <- auto.arima(kingston_msts, seasonal = TRUE, trace = TRUE , D=1)
kingstonarima_pred <- forecast(kingstonarima, h=1093, level=c(0.8,0.90))
plot(kingstonarima_pred)



##  Cross Validation ---------------------------------------------------


kingston_train <- subset(tidykingston, Year <= 1999) # split train and test data by 1999 
kingston_test <- subset(tidykingston, Year > 1999)
kingston_msts_train <- msts(kingston_train$Temp, seasonal.periods = c(12,120))
kingstonarima_train <- auto.arima(kingston_msts_train, seasonal = TRUE, trace = TRUE, D = 1)
kingstonarima_train_predict <- forecast(kingstonarima_train, h=240, level=c(0.9,0.95))
plot(kingstonarima_train_predict, xlab = 'Year', ylab = "Predicted Temp.")


kingstonarima_pred_date <- data.frame("Date" =seq(as.Date("2000/1/1"), by = "month", length.out =240))
kingstonarima_pred_train_final <- cbind(kingstonarima_pred_date,kingstonarima_train_predict)
kingstonarima_pred_final_data <- data.frame("Date" =kingstonarima_pred_train_final$Date,"forecast temp." = kingstonarima_pred_train_final$`Point Forecast`) 
kingstonarima_pred_final_data$ActualTemp <- kingston_test$Temp
mape_kingstonarima_pred <- MLmetrics::MAPE(kingstonarima_pred_final_data$forecast.temp.,kingstonarima_pred_final_data$ActualTemp) 
mape_kingstonarima_pred


##  Write answers to CSV -----------------------------------------------

kingston_date <- data.frame("Date" =seq(as.Date("2010/1/1"), by = "month", length.out =1093))
kingston_prediction <- cbind(kingston_date,kingstonarima_pred)
write.csv(kingston_prediction, "kingston_prediction.csv", row.names = FALSE)



Kingston_Predict <- data.frame("Date" = kingston_prediction$Date, "point_forecast" = kingston_prediction$`Point Forecast`, "lo_90" = kingston_prediction$`Lo 90`, "hi_90" = kingston_prediction$`Hi 90`)

Jan_2030 <- Kingston_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2030-01-01")
Jul_2030 <- Kingston_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2030-07-01")
Jan_2050 <- Kingston_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2050-01-01")
Jul_2050 <- Kingston_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2050-07-01")
Jan_2100 <- Kingston_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2100-01-01")
Jul_2100 <- Kingston_Predict %>% dplyr::select(Date, point_forecast, lo_90, hi_90) %>% filter(Date == "2100-07-01")

Kingston_Predict <- rbind(Jan_2030,Jul_2030,Jan_2050,Jul_2050,Jan_2100,Jul_2100)
write.csv(Kingston_Predict, "Kingston Q3 Answers.csv", row.names = FALSE)


# Q4 using Tableau ####

#Q5 -------
#### Q5: NASA ####
# combine GLB and predction files into one:
# 1. original data processed as "GLB2"

GLB2
view(GLB2)
GLB3 <-GLB2
GLB3$Date <- paste(GLB3$Year, GLB3$Month, sep="-") %>% ym() %>% as.Date()
GLB3 <- GLB3[-c(1:2)] # drop the rest of columns
#view(GLB3)
GLB4 <- GLB3[, c(2,1)] #reorder colums so we can bind them
GLB4
#view(GLB4)
GLB4 <- GLB4[with(GLB4, order(Date, Temperature)), ]  #order by date
nasa_all <- GLB4

# add decade numbers into the nasa_all dataset
nasa_all1 <- nasa_all
newcol <- rep(c(rep(1,times =120),rep(2, times = 120) , rep(3, times =120),
                rep(4,times = 120), rep(5, times = 120), rep(6,times =120),
                rep(7,times = 120),rep(8,times=120), rep(9,times = 120) ,
                rep(10,times = 120),rep(11,times=120), rep(12,times = 120) ,
                rep(13,times = 120),rep(14,times=120) ,rep(15,times=120), times=120))
newcol2 <- newcol[1:1695]
nasa_all1$Decade <- newcol2
str(nasa_all1)
nasa_all1$Decade <- as.factor(nasa_all1$Decade)

# add decades with meaningfull names into the nasa_all dataset
newColDecade <- rep(c(rep('1880s',times =120),rep('1890s', times = 120) , rep('1900s', times =120),
                      rep('1910s',times = 120), rep('1920s', times = 120), rep('1930s',times =120),
                      rep('1940s',times = 120),rep('1950s',times=120), rep('1960s',times = 120) ,
                      rep('1970s',times = 120),rep('1980s',times=120), rep('1990s',times = 120) ,
                      rep('2000s',times = 120),rep('2010s',times=120), rep('2020s',times=120),times=120))

newcol3 <- newColDecade[1:1695]
nasa_all1$Decades <- newcol3
nasa_all1$Decades <- as.factor(nasa_all1$Decades)

nasa_all <- nasa_all1 
view(nasa_all)
str(nasa_all)

# now split nasa data into two seperate sets 1880-2013 and 2013-2021
nasa_all[1:1596, ]
nasa_2013 <- nasa_all[1:1596, ]
view(nasa_2013)

nasa_2021 <- nasa_all[1597:1695,]
view(nasa_2021)

# Nasa plots 2013
plot(nasa_2013)

g <- ggplot(nasa_2013, aes(Decades, Temperature)) +
  geom_point() + geom_smooth(method = "lm") 
print(g)
# + layer smooth function with lr line

#### plot standard deviation 
# ggplot(nasa_all) + 
#   geom_histogram(aes(x = Temperature, fill = Decades, position = 'identity', alpha=0.9))


# KEEP Decade Average temperature 1800 - 2010
ggplot(nasa_all,aes(x = Decades, y = Temperature)) +
  geom_point(colour = "blue") +
  geom_smooth(colour = "red",size = 1) +
  scale_y_continuous(limits = c(13,15)) +
  ggtitle ("Global Temperature by Decade 1880s - 2020s") +
  xlab("Decades") +  ylab ("Temperature ( ºC )")

#### KEEP BOX Plot ####
# calculate mean for each decade and and graph is using  boxplot 
M = tapply(nasa_2013$Temperature,
           INDEX = nasa_2013$Decades,
           FUN   = mean)
boxplot(Temperature ~ Decades, data=nasa_2013, col.axis="blue",
        main= ("Global Temperature in Decades (1880s - 2013s)"),
        sub = "Temperature is treanding upwards every decade",
        ylab="Average Temperature (C)",
        xlab = "Decades",
        cex.main = 2,   font.main= 2, col.main= "darkblue",
        cex.sub = 0.75, font.sub = 3, col.sub = "darkblue",
        col.lab ="darkblue")
grid(nx=20, ny=10)
points(M,
       col="red",
       pch="x",
       cex=1)

#### NASA calculate STD for decades and years ####
# install.packages("psych")
#install.packages("writexl")
library(writexl)
library("psych")
#STD Decades
decade_vals <- psych::describeBy(nasa_all$Temperature, group = nasa_all$Decades) # add decade values to df
nasa_sd_all <- do.call(rbind.data.frame, decade_vals)
nasa_sd_all$sd
decades <- rownames(nasa_sd_all)
nasa_sd_all <- cbind(nasa_sd_all,decades)
nasa_sd_all <-tibble(nasa_sd_all)

nasa_sd_2013 <- nasa_sd_all[1:14,] #1800s to 2010s
nasa_sd_2020 <- nasa_sd_all[15,] #2010-2020
# Note: since we cannot plot only one decade of varation between 2013 - 2020, we will plot yearly variations
# write.csv(nasa_sd_2013, "NASA STD 1880s-2010s Decades.csv", row.names = FALSE)
# write_xlsx(nasa_sd_2013,"NASA STD 1880s-2010s Decades.xlsx")

ggplot(data=nasa_sd_all, aes(x=decades, y=sd, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point(colour = "red")+
  geom_smooth(colour = "blue",size = 0.5) +
  scale_y_continuous(limits = c(0.12, 0.19)) +
  ggtitle ("Temperature Variation in Decades 1880s - 2020s") +
  xlab("Decades") +  ylab ("Temperature Variation ( ºC )")


ggplot(data=nasa_sd_2013, aes(x=decades, y=sd,group=1)) +
  geom_line(linetype = "dashed")+
  geom_point(colour = "red")+
  geom_smooth(colour = "blue",size = 0.5) +
  scale_y_continuous(limits = c(0.12, 0.19)) +
  ggtitle ("Temperature Variation in Decades 1880s - 2010s") +
  xlab("Decades") + ylab ("Temperature Variation ( ºC )") + 

# not enough data for one decade, cannot be plotted 
ggplot(data=nasa_sd_2020, aes(x=decades, y=sd,group=1)) +
  geom_line(linetype = "dashed")+
  geom_point(colour = "blue")+
  geom_smooth(colour = "red",size = 0.5) +
  scale_y_continuous(limits = c(0.12, 0.19)) +
  ggtitle ("Temperature Variation in Decades 2013 - 2021s") +
  xlab("Decades") + ylab ("Temperature Variation ( ºC )")


# STD calc for years
# data from 2013 to 2021 barely passes a decade, we calcualte STD for each year 
# nasa_2021 <- nasa_all[1597:1695,]
view(nasa_2021)
newcolyr <- rep(c(rep(1,times =12),rep(2, times = 12) , rep(3, times =12),
               rep(4,times = 12), rep(5, times = 12), rep(6,times =12),
               rep(7,times = 12),rep(8,times=12), rep(9,times=12), times=12))
newcol2 <- newcolyr[1:99]
nasa_2021$Year <- newcol2
nasa_2021$Year<- as.factor(nasa_2021$Year)

nasa_2021$YearName <- substr(as.character(nasa_2021$Date), 1, 4)

view(nasa_2021)

year_vals <- psych::describeBy(nasa_2021$Temperature, group = nasa_2021$YearName) # add year values to df
nasa_sd_year <- do.call(rbind.data.frame, year_vals)
nasa_sd_year$sd
years <- rownames(nasa_sd_year)
nasa_sd_year <- cbind(nasa_sd_year,years)
nasa_sd_year <-tibble(nasa_sd_year)
#write_xlsx(nasa_sd_year,"NASA STD 2013-2021s Years.xlsx")
tibble(nasa_sd_year)
view(nasa_sd_year)
str(nasa_sd_year)
nasa_sd_year <- nasa_sd_year[1:8, ] #remove 2021 data (only 3 months)

ggplot(data=nasa_sd_year, aes(x=years, y=sd,group=1)) +
  geom_line(linetype = "dashed")+
  geom_point(colour = "red")+
  geom_smooth(colour = "blue",size = 0.5) +
  scale_y_continuous(limits = c(0, 0.2)) +
  ggtitle ("Global Annual Temperature Variation 2013 - 2020") +
  xlab("Years") + ylab ("Temperature Variation ( ºC )") 


#### moving average calculations ####
# data: nasa_all
#install.packages("ggfortify")
library("TTR")
#conver nasa_all to ts on a decade and yearly basis
view(nasa_all)
nasa_all_decades_ts <- ts(nasa_all$Temperature,start=1880, frequency=120) # all years in decade
nasa_all_years_ts <- ts(nasa_2013$Temperature,start=1880, frequency=12) 
nasa_all_year2013_20_ts <- ts(nasa_2021$Temperature,start=2013, frequency=12) 
nasa_all_year2010_20_ts <- ts(nasa_2021$Temperature,start=2010, frequency=12)

# nasa_all_years_ts <- ts(nasa_all$Temperature,start=1880, frequency=12) # all year
# sma1 = SMA(nasa_all_years_ts,n=120) #n=120 converts to decade
# plot.ts(sma1,col='red',main='Global Temperature Moving Average (10 Year) for NASA')
# sma1 = SMA(nasa_all_years_ts,n=12) #n=120 converts to decade
# plot.ts(sma1,col='red',main='Global Temperature Moving Average (1 Year) for NASA')
##
#sma1 = SMA(nasa_all_years_ts,n=12) #n=120 converts to decade
# plot.ts(nasa_all_decades_ts,col='red',main='Global Temperature Moving Average (1 Year) for NASA')
# plot.ts(nasa_all_years_ts,col='red',main='Global Temperature Moving Average (1 Year) for NASA')
# plot.ts(nasa_all_year2013_20_ts,col='red',main='Global Temperature Moving Average (1 Year) for NASA')
# plot.ts(nasa_all_year2010_20_ts,col='red',main='Global Temperature Moving Average (1 Year) for NASA')


# all decades
autoplot(nasa_all_decades_ts) + xlab("Years") + ylab("Temperature") +
  ggtitle("Glob. Temp Per Decade")

# 1880 to 2013 years
# autoplot(nasa_all_year2013_ts) + xlab("Years") + ylab("Temperature") +
#   ggtitle("Glob. Temp Per Year 1880 - 2013")
# 
# # 1880 to 2013 years
# autoplot(nasa_all_year2013_20_ts) + xlab("Years") + ylab("Temperature") +
#   ggtitle("Glob. Temp Per Year 2013 - 2020")

# NASA MA decade MA from 1880 to 2020
autoplot(nasa_all_decades_ts, series = "NASA") + 
  autolayer(ma(nasa_all_year_all_ts, order = 1, centre = TRUE), series = "1-decade MA") +
  labs(x = "Decade", y = "Temperature") + 
  ggtitle("NASA Decade Moving Tempreture: 1880 - 2013")

# NASA MA monthly avereage  MA from 1880 to 2020
autoplot(nasa_all_years_ts, series = "NASA") + 
  autolayer(ma(nasa_all_years_ts, order = 13, centre = TRUE), series = "13-month MA") +
  labs(x = "Decade", y = "Temperature") + 
  ggtitle("NASA Monthly Moving Tempreture: 1880 - 2020")


# NASA MA monthly avereage  MA from 2013 - 2020
autoplot(nasa_all_year2013_20_ts, series = "NASA") + 
  autolayer(ma(nasa_all_year2013_20_ts, order = 13, centre = TRUE), series = "13-month MA") +
  labs(x = "Month", y = "Temperature") + 
  ggtitle("Month Moving Tempreture: 2013 - 2020")



# NASA MA monthly avereage  MA from 2013 - 2020
autoplot(nasa_all_year2010_20_ts, series = "NASA") + 
  autolayer(ma(nasa_all_year2010_20_ts, order = 13, centre = TRUE), series = "13-month MA") +
  labs(x = "Month", y = "Temperature") + 
  ggtitle("Month Moving Tempreture: 2010 - 2020")

# 
# 
# 
# autoplot(MET_msts, series="Avg Temp")+
#   autolayer(ma(nasa_all_years_ts,12, series = "12 MA")+
#               xlab("Year") + ylab("Temp Anomaly") +
#               ggtitle("Avg Yearly Temp Anomaly"))
# 
# 
# # ma is moving average, df is the name of your time series and 
# # 5 is the number of periods. If 5, it'll use 2 periods before and 2 after.
# autoplot(climate_nasa_ts, series="Avg Anomaly")+
#   autolayer(ma(climate_nasa_ts,5, series = "5-MA")+
#               xlab("Year") + ylab("Temp Anomaly") +
#               ggtitle("Avg Yearly Temp Anomaly") +
#               scale_colour_manual(values=c("Avg Anomaly"="green","5-MA"="red"),
#                                   breaks=c("Avg Anomaly","5-MA"))
# 
# 
# 
# autoplot(elecsales, series="Data") +
#   autolayer(ma(elecsales,5), series="5-MA") +
#   xlab("Year") + ylab("GWh") +
#   ggtitle("Annual electricity sales: South Australia") +
#   scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
#                       breaks=c("Data","5-MA"))
# 
# 


#### Q5 MET ####

#### MET ####
# combine GLB and predction files into one:
# 1. original data processed as "GLB2"
# 2. "nasa_prediction.csv", the NASA auto ARIMA mts model prediction file 
# file to use: "Hadcrut4_Processed.csv"
# NASA<-read.csv(file.choose(), header=TRUE, sep=",", skip=1) 
# NASA$Temperature <- as.numeric(NASA$Temperature ) + 14
met <-read.csv("Hadcrut4_Processed.csv", header=TRUE, sep=",", skip=1) 

met
view(met)

# rename colums
view(met)
colnames(met)[1] <- "Year"
colnames(met)[2] <- "Month"
colnames(met)[3] <- "Temperature"

met$Temperature <- met$Temperature + 14

met$Date <- paste(met$Year, met$Month, sep="-") %>% ym() %>% as.Date()
view(met)

met <- met[with(met, order(Date, Temperature)), ]  #order by date
#view(MET3)
met_all <- met
view(met_all)

#+++++++
met_all <- met3
newcol <- rep(c(rep(1,times =12),rep(2, times = 12) , rep(3, times =12),
                rep(4,times = 12), rep(5, times = 12), rep(6,times =12),
                rep(7,times = 12),rep(8,times=12), rep(9,times = 12) ,
                rep(10,times = 12)),times=18)

newcol2<-newcol[1:2052]
met3$Decade <- newcol2
str(met3)
view(met3)
met3$Decade <- as.factor(met3$Decade)
#met3$X <- as.factor(met3$X)
str(met3)
summary(met3)
view(met3)
# MET_msts <- msts(met3$Temperature, seasonal.periods=c(12,120)) 
# str(MET_msts)

#++++++
# add decades into the nasa_all dataset
# met_all1 <- met_all
# newcol <- rep(c(rep(1,times =120),rep(2, times = 120) , rep(3, times =120),
#                 rep(4,times = 120), rep(5, times = 120), rep(6,times =120),
#                 rep(7,times = 120), rep(8, times = 120), rep(9,times =120),
#                 rep(10,times = 120), rep(11, times = 120), rep(12,times =120),
#                 rep(13,times = 120), rep(14,times = 120), rep(15,times = 120),
#                 rep(16,times = 120), rep(17,times = 120),rep(18,times = 120), times=120))
# newcol2 <- newcol[1:2053] 
#met_all1$Decade <- newcol2
#str(met_all1)
# met_all1$Decade <- as.factor(met_all1$Decade)
# view(met_all1)

met_all <- met3 
# add decades with meaningfull names into the nasa_all dataset
newColDecade <- rep(c(rep('1850s',times =120),rep('1860s', times = 120) , rep('1870s', times =120),
                      rep('1980s',times = 120), rep('1890s', times = 120), rep('1900s',times =120),
                      rep('1910s',times = 120),rep('1920s',times=120), rep('1930s',times = 120) ,
                      rep('1940s',times = 120),rep('1950s',times=120), rep('1960s',times = 120) ,
                      rep('1970s',times = 120),rep('1980s',times=120), rep('1990s',times = 120) ,
                      rep('2000s',times = 120),rep('2010s',times=120), rep('2020s',times = 120) , times=120))
newcol3 <- newColDecade[1:2052]
met_all$Decades <- newcol3
met_all$Decades <- as.factor(met_all$Decades)


view(met_all)
str(met_all)

# MET plots
plot(met_all)
g <- ggplot(met_all, aes(Decades, Temperature)) #data (x,y) and aes done
g + geom_point() + geom_smooth() # add smooth function
g + geom_point() + geom_smooth(method = "lm") # + layer smooth function with lr line

#### plot standard deviation 

# Decade Average temperature 1800 - 2100
ggplot(met_all,aes(x = Decades, y = Temperature)) +
  geom_point(colour = "blue") +
  geom_smooth(colour = "red",size = 1) +
  scale_y_continuous(limits = c(13,15.5)) +
  ggtitle ("Global Temperature 1850 - 2100 (MET)") +
  xlab("Decades") +  ylab ("Average Temperature ( ºC )")

# # Distribution of the average temperature density plot
# ggplot(met_all,aes(x = Temperature, colour = Decades)) +
#   geom_density() +
#   scale_x_continuous(limits = c(13,17.5)) +
#   ggtitle ("Temperature distribution by Decades") +
#   xlab("Distribution of the Average Temperature ( ºC )") +  ylab ("Probability")

# calculate mean for each decade and and graph is using  boxplot 
M = tapply(met_all$Temperature,
           INDEX = met_all$Decades,
           FUN   = mean)
boxplot(Temperature ~ Decades, data=met_all, col.axis="blue",
        main= ("Global Temperature in Decades (1850s - 2013s)"),
        sub = "MET data also shows the Tempreature is treanding upwards every decade",
        ylab="Average Temperature (C)",
        xlab = "Decades",
        cex.main = 2,   font.main= 1, col.main= "darkblue",
        cex.sub = 0.75, font.sub = 1, col.sub = "darkblue",
        col.lab ="darkblue")
grid(nx=20, ny=10)
points(M,
       col="red",
       pch="x",
       cex=1)

#### MET calculate STD for decades and years ####
# library(writexl)
# library("psych")
#STD Decades
view(met_all)
decade_vals <- psych::describeBy(met_all$Temperature, group = met_all$Decades) # add decade values to df
met_sd_all <- do.call(rbind.data.frame, decade_vals)
met_sd_all$sd
decades <- rownames(met_sd_all)
met_sd_all <- cbind(met_sd_all,decades)
met_sd_all <-tibble(met_sd_all)
view(met_sd_all)

met_sd_2013 <- met_sd_all[1:16,] #1850s to 2010s
met_sd_2020 <- met_sd_all[17,] #2010-2020
# Note: since we cannot plot only one decade of varation between 2013 - 2020, we will plot yearly variations
#write.csv(nasa_sd_2013, "MET STD 1880s-2010s Decades.csv", row.names = FALSE)
# write_xlsx(met_sd_2013,"MET STD 1880s-2010s Decades.xlsx")
# write_xlsx(met_sd_2020,"MET STD 2010 - 2013 Decades.xlsx")

ggplot(data=met_sd_all, aes(x=decades, y=sd, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point(colour = "red")+
  geom_smooth(colour = "blue",size = 0.5) +
  scale_y_continuous(limits = c(0.12, 0.19)) +
  ggtitle ("Temperature Variation in Decades 1850s - 2020s MET") +
  xlab("Decades") +  ylab ("Temperature Variation ( ºC )")


ggplot(data=met_sd_2013, aes(x=decades, y=sd, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point(colour = "red")+
  geom_smooth(colour = "blue",size = 0.5) +
  scale_y_continuous(limits = c(0.12, 0.19)) +
  ggtitle ("Temperature Variation in Decades 1880s - 2010s MET") +
  xlab("Decades") + ylab ("Temperature Variation ( ºC )")


# not enough data for one decade, cannot be plotted 
ggplot(data=met_sd_2020, aes(x=decades, y=sd,group=1)) +
  geom_line(linetype = "dashed")+
  geom_point(colour = "blue")+
  geom_smooth(colour = "red",size = 0.5) +
  scale_y_continuous(limits = c(0.12, 0.19)) +
  ggtitle ("Temperature Variation in Decades 2013 - 2021s") +
  xlab("Decades") + ylab ("Temperature Variation ( ºC )")


# MWR STD calc for years
# data from 2013 to 2021 barely passes a decade, we calcualte STD for each year 
view(met_all)
met_2021 <- met_all[1956:2052,] #2013 to 2021


view(met_2021)
# newcolyr <- rep(c(rep(1,times =12),rep(2, times = 12) , rep(3, times =12),
#                   rep(4,times = 12), rep(5, times = 12), rep(6,times =12),
#                   rep(7,times = 12),rep(8,times=12), rep(9,times=12), times=12))
# newcol2 <- newcolyr[1:99]
# nasa_2021$Year <- newcol2
# nasa_2021$Year<- as.factor(nasa_2021$Year)

met_2021$YearName <- substr(as.character(met_2021$Date), 1, 4)

view(met_2021)

# calcuate STD for year
met_year_vals <- psych::describeBy(met_2021$Temperature, group = met_2021$YearName) # add year values to df
met_sd_year <- do.call(rbind.data.frame, met_year_vals)
met_sd_year$sd
years <- rownames(met_sd_year)
met_sd_year <- cbind(met_sd_year,years)
met_sd_year <-tibble(met_sd_year)
view(met_sd_year)
write_xlsx(met_sd_year,"MET STD 2013-2021s Years.xlsx")
tibble(met_sd_year)
view(met_sd_year)
str(met_sd_year)

met_sd_year <- met_sd_year[1:8, ] #remove 2021 data (only 1 months)

ggplot(data=met_sd_year, aes(x=years, y=sd,group=1)) +
  geom_line(linetype = "dashed")+
  geom_point(colour = "red")+
  geom_smooth(colour = "blue",size = 0.5) +
  scale_y_continuous(limits = c(0, 0.2)) +
  ggtitle ("MET Global Annual Temperature Variation 2013 - 2020") +
  xlab("Years") + ylab ("Temperature Variation ( ºC )") 



#### moving average calc MET ####
#### moving average calculations ####
# data: nasa_all
#install.packages("ggfortify")
library("TTR")
#conver nasa_all to ts on a decade and yearly basis
view(met_all)
met_all_decades_ts <- ts(met_all$Temperature,start=1850, frequency=120) # all years
met_all_years_ts <- ts(met_all$Temperature,start=1850,frequency=12) 
met_all_years_1850_2013_ts <- ts(met_all$Temperature,start=1850, end = 2013,frequency=12) 
met_all_year2013_20_ts <- ts(met_all$Temperature,start=2013, frequency=12) 
met_all_year2010_20_ts <- ts(met_all$Temperature,start=2010, frequency=12)

sma1_met = SMA(met_all_years_ts,n=120) #n=120 converts to decade
plot.ts(sma1_met,col='red',main='Global Temperature Moving Average (10 YEARS) for MET')

# # all decades
# autoplot(met_all_decades_ts) + xlab("Decades") + ylab("Temperature") +
#   ggtitle("Glob. Temperature Moving Average Per Decade (MET)")

# MET MA decade MA from 1850 to 2013
autoplot(met_all_years_ts, series = "MET") + 
  autolayer(ma(met_all_years_ts, order = 13, centre = TRUE), series = "1-decade MA") +
  labs(x = "Decade", y = "Temperature") + 
  ggtitle("MET Moving Average Tempreture Decade: 1880 - 2013")

# NASA MA monthly avereage  MA from 1850 to 2020
autoplot(met_all_years_ts, series = "NASA") + 
  autolayer(ma(met_all_years_ts, order = 13, centre = TRUE), series = "13-month MA") +
  labs(x = "Year", y = "Temperature") + 
  ggtitle("MET Monthly Moving Tempreture: 1850 - 2020")


# MET MA monthly avereage  MA from 2013 - 2020
autoplot(met_all_year2013_20_ts, series = "MET") + 
  autolayer(ma(met_all_year2013_20_ts, order = 13, centre = TRUE), series = "13-MA") +
  labs(x = "Month", y = "Temperature") + 
  ggtitle("Month Moving Tempreture: 2013 - 2020 MET")


# MET MA monthly avereage  MA from 1850 - 2013
autoplot(met_all_years_1850_2013_ts, series = "MET") + 
  autolayer(ma(met_all_years_1850_2013_ts, order = 13, centre = TRUE), series = "13-MA") +
  labs(x = "Month", y = "Temperature") + 
  ggtitle("Month Moving Tempreture: 1850 - 2013 MET")

# MET MA monthly avereage  MA from 2010 - 2020
autoplot(met_all_year2010_20_ts, series = "MET") + 
  autolayer(ma(met_all_year2010_20_ts, order = 13, centre = TRUE), series = "13-MA") +
  labs(x = "Month", y = "Temperature") + 
  ggtitle("Month Moving Tempreture: 2010 - 2020 MET")


met_all_year2005_20_ts <- ts(met_all$Temperature,start=2005, frequency=12)
# MET MA monthly avereage  MA from 2010 - 2020
autoplot(met_all_year2005_20_ts, series = "MET") + 
  autolayer(ma(met_all_year2005_20_ts, order = 13, centre = TRUE), series = "13-MA") +
  labs(x = "Month", y = "Temperature") + 
  ggtitle("Month Moving Tempreture: 2005 - 2020 MET")


GLB2$Date<-as.Date(paste0(GLB2$Year,'-',GLB2$Month,'-01'), format="%Y-%m-%d")
GLB2<-subset(GLB2,select=c(Date,Temperature))

CLB3 <- GLB2
newcol <- rep(c(rep(1,times =12),rep(2, times = 12) , rep(3, times =12),
                rep(4,times = 12), rep(5, times = 12), rep(6,times =12),
                rep(7,times = 12),rep(8,times=12), rep(9,times = 12) ,
                rep(10,times = 12)),times=17)


newcol2<-newcol[1:1695]
CLB3$Yearin10 <- newcol2
str(CLB3)
CLB3$Yearin10 <- as.factor(CLB3$Yearin10)
GLB_msts <- msts (CLB3$Temperature, seasonal.periods=c(12,120)) 

#Q6 - NASA####
nasa_train <-subset(CLB3,subset=CLB3$Date<'2008-01-01') #1536
nasa_test <- subset(CLB3,subset=CLB3$Date>='2008-01-01')
nasa_test <- subset(nasa_test,subset=nasa_test$Date<='2017-12-01') #120

nasa_msts_train <- msts(nasa_train$Temperature, seasonal.periods = c(12,120))
nasa_msts_test<- msts(nasa_test$Temperature, seasonal.periods = c(12,120))
nasa_arima2_train <-Arima(nasa_msts_train,order=c(3,1,1),seasonal=c(0,1,0))
nasa_arima2_train_pridect <- forecast(nasa_arima2_train, h=120, level=c(0.9,0.95))



# Convert forecasts to data frames
GLB_msts_arima2_df <- cbind( as.data.frame(nasa_arima2_train_pridect))  # Creating a data frame
GLB_msts_arima2_df$Date <- nasa_test$Date
summary(GLB_msts_arima2_df)

##plot1 overvall----
plot(CLB3$Date,CLB3$Temperature,xlab = 'Year', ylab = "Temperature",type='l',main ="Forcasts for Temperature 2007-2017")
lines(GLB_msts_arima2_df$Date,GLB_msts_arima2_df$`Point Forecast`,col="red")

#average temp in 2007---14.6675
Amstrong<- data.frame(cbind("Date"= nasa_test$Date,"Temperature"=rep(14.6675,120)))

nasa_fit1 <- meanf(nasa_msts_train,h=120)
nasa_fit2 <- rwf(nasa_msts_train,h=120)
nasa_fit3 <- snaive(nasa_msts_train,h=120)

nasa_fit1_df <- cbind( as.data.frame(nasa_fit1))  # Creating a data frame
nasa_fit1_df$Date <- nasa_test$Date
nasa_fit2_df <- cbind( as.data.frame(nasa_fit2))  # Creating a data frame
nasa_fit2_df$Date <- nasa_test$Date
nasa_fit3_df <- cbind( as.data.frame(nasa_fit3))  # Creating a data frame
nasa_fit3_df$Date <- nasa_test$Date

##plot2 actual and predict data----

plot(nasa_test$Date, nasa_test$Temperature,type='l',xlab="Year", ylab="Temp",col="Black",main ="Predictions in different models",ylim=c(13.8,15.8))
lines(Amstrong$Date, Amstrong$Temperature,col="orange")
lines(GLB_msts_arima2_df$Date, GLB_msts_arima2_df$`Point Forecast`,col="red")
lines(nasa_fit1_df$Date, nasa_fit1_df$`Point Forecast`,col="purple")
lines(nasa_fit2_df$Date, nasa_fit2_df$`Point Forecast`,col="blue")
lines(nasa_fit3_df$Date, nasa_fit3_df$`Point Forecast`,col="pink")
legend("topleft", legend=c("Actual", "ARIMA(3,1,1)(0,1,0)[120]","Amstrong(Year)" ,"Average method","Naïve method(Month)","Seasonal naïve method"),
       col=c("black", "red","orange","purple","blue","pink"), lty=1:2, cex=0.8)

##plot3 overvall----
plot(CLB3$Date,CLB3$Temperature,xlab = 'Year', ylab = "Temperature",type='l',main ="Forcasts for Temperature 2007-2017")
lines(GLB_msts_arima2_df$Date, GLB_msts_arima2_df$`Point Forecast`,col="red")
lines(Amstrong$Date, Amstrong$Temperature,col="orange")
lines(nasa_fit1_df$Date, nasa_fit1_df$`Point Forecast`,col="purple")
lines(nasa_fit2_df$Date, nasa_fit2_df$`Point Forecast`,col="blue")
lines(nasa_fit3_df$Date, nasa_fit3_df$`Point Forecast`,col="pink")
legend("topleft", legend=c("Actual", "ARIMA(3,1,1)(0,1,0)[120]","Amstrong(Year)" ,"Average method","Naïve method(Month)","Seasonal naïve method"),
       col=c("black", "red","orange","purple","blue","pink"), lty=1:2, cex=0.8)

accuracy(GLB_msts_arima2_df$`Point Forecast`,nasa_test$Temperature)
#ME      RMSE       MAE        MPE     MAPE
#Test set -0.01208363 0.1976881 0.15274 -0.09401635 1.036037
accuracy(Amstrong$Temperature,nasa_test$Temperature)
#ME      RMSE       MAE       MPE     MAPE
#Test set 0.07858333 0.2025854 0.1498333 0.5171073 1.008252
accuracy(nasa_fit1_df$`Point Forecast`,nasa_test$Temperature)
#ME      RMSE       MAE      MPE     MAPE
#Test set 0.7721641 0.7944198 0.7721641 5.221346 5.221346
accuracy(nasa_fit2_df$`Point Forecast`,nasa_test$Temperature)
#ME      RMSE       MAE        MPE     MAPE
#Test set 0.2460833 0.3089053 0.2534167 1.653183 1.704195
accuracy(nasa_fit3_df$`Point Forecast`,nasa_test$Temperature)
#ME      RMSE    MAE      MPE     MAPE
#Test set 0.1759167 0.2730156 0.22875 1.180076 1.544664

library(Metrics)
#Mean absolute scaled error:When comparing forecasting methods, the method with the lowest MASE is the preferred method.
mase(nasa_test$Temperature, GLB_msts_arima2_df$`Point Forecast`) #1.568254
mase(nasa_test$Temperature, Amstrong$Temperature) #1.53841
mase(nasa_test$Temperature, nasa_fit1_df$`Point Forecast`) #7.928173
mase(nasa_test$Temperature, nasa_fit2_df$`Point Forecast`)#2.601949
mase(nasa_test$Temperature, nasa_fit3_df$`Point Forecast`)#2.348684



## RMSE plot----
e <- tsCV(nasa_msts_test, rwf, drift=TRUE, h=120)
sqrt(mean(e^2, na.rm=TRUE))
#0.3032744
sqrt(mean(residuals(rwf(nasa_msts_train, drift=TRUE))^2, na.rm=TRUE))
#0.1207451


par(mfrow=c(1,1)) 
far2 <- function(x, h){forecast(Arima(x, order=c(3,1,1)), h=h)}

e1 <- tsCV(nasa_msts_test, far2, h=120)
mse1 <- colMeans(e1^2, na.rm = T)

# Plot the MSE values against the forecast horizon
visual1<-data.frame(h = 1:120, MSE = mse1) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()+labs(title="MSE plot for Arima method")

e2 <- tsCV(nasa_msts_test, naive, h=120)
# Compute the MSE values and remove missing values
mse2 <- colMeans(e2^2, na.rm = T)
# Plot the MSE values against the forecast horizon
visual2<-data.frame(h = 1:120, MSE = mse2) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()+labs(title="MSE plot for naive method")


visual1<-data.frame(h = 1:120, MSE = mse1)
visual2<-data.frame(h = 1:120, MSE = mse2)
plot(visual1$h,visual1$MSE,col="red",main="MSE plot for ARIMA & Naive")
lines(visual2$h,visual2$MSE,col="blue")
legend("topleft", legend=c("Arima", "Naive"),
       col=c("red","blue"), lty=1:2, cex=0.8)


##cross validation----------------

##  ARIMA ------------------------------------
nasa_2017 <- subset(CLB3,subset=CLB3$Date<='2017-12-01') #1656 

nasa_msts_2017 <- msts(nasa_2017$Temperature, seasonal.periods = c(12,120))
accuracy.arima=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:10)
{
  nTest <- 12*i
  nTrain <- length(nasa_msts_2017)- nTest -1
  train <- window(nasa_msts_2017, start=1, end=1+(nTrain)/(120))
  test <- window(nasa_msts_2017, start=1+(nTrain+1)/(120), end=1+(nTrain+12)/(120))
  trainlm <- Arima(nasa_msts_2017,order=c(3,1,1),seasonal=c(0,1,0))
  trainlmf <- forecast(trainlm,h=120)
  sp<- as.numeric(trainlmf$mean)
  
  cat("----------------------------------
      Data Partition",i,"
      Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
      Test Set includes 12 time periods. Observations", nTrain+1, "to", nTrain+12,"
      ")
  print(accuracy(sp,test))
  #  print(residauto)
  accuracy.arima<-rbind(accuracy.arima,accuracy(sp,test)[1,5])
  #print(sp$model)
}

accuracy.arima<-accuracy.arima[-1]


mean(accuracy.arima) #1.244351
sd(accuracy.arima) #0.3675746

##  Amstrong ------------------------------------

accuracy.Amstrong=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:10)
{
  nTest <- 12*i
  nTrain <- length(nasa_msts_2017)- nTest -1
  train <- window(nasa_msts_2017, start=1, end=1+(nTrain)/(120))
  test <- window(nasa_msts_2017, start=1+(nTrain+1)/(120), end=1+(nTrain+12)/(120))
  sp<- as.numeric(Amstrong$Temperature)
  
  cat("----------------------------------
      Data Partition",i,"
      Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
      Test Set includes 12 time periods. Observations", nTrain+1, "to", nTrain+12,"
      ")
  print(accuracy(sp,test))
  #  print(residauto)
  accuracy.Amstrong<-rbind(accuracy.Amstrong,accuracy(sp,test)[1,5])
  #print(sp$model)
}

accuracy.Amstrong<-accuracy.Amstrong[-1]

mean(accuracy.Amstrong) # 1.008252
sd(accuracy.Amstrong) #0.6433732



write.csv(nasa_arima2_train_pridect, "nasa_pred_Q6.csv", row.names = FALSE)
write.csv(nasa_fit1, "nasa_pred2_Q6.csv",row.names = FALSE)
write.csv(nasa_fit2, "nasa_pred3_Q6.csv",row.names = FALSE)
write.csv(nasa_fit3, "nasa_pred4_Q6.csv",row.names = FALSE)

#Q6 MET---------------------------------------
MET<-read.csv(file.choose(), header=TRUE) 

MET$Date<-as.Date(paste0(MET$Year,'-',MET$Month,'-01'), format="%Y-%m-%d")
MET2<-subset(MET,select=c(Date,Median))
MET2$Median <- MET2$Median +14
MET3 <- MET2

newcol <- rep(c(rep(1,times =12),rep(2, times = 12) , rep(3, times =12),
                rep(4,times = 12), rep(5, times = 12), rep(6,times =12),
                rep(7,times = 12),rep(8,times=12), rep(9,times = 12) ,
                rep(10,times = 12)),times=17)


newcol2<-newcol[1:2053]
MET3$Yearin10 <- newcol2
str(MET3)
MET3$Yearin10 <- as.factor(MET3$Yearin10)
GLB_msts <- msts (MET3$Median, seasonal.periods=c(12,120)) 

##ARIMA(3,1,1)(0,1,0)####
met_train <-subset(MET3,subset=MET3$Date<'2008-01-01') #1536
met_test <- subset(MET3,subset=MET3$Date>='2008-01-01')
met_test <- subset(met_test,subset=met_test$Date<='2017-12-01') #120

met_msts_train <- msts(met_train$Median, seasonal.periods = c(12,120))
met_arima2_train <-Arima(met_msts_train,order=c(3,1,1),seasonal=c(0,1,0))
met_arima2_train_pridect <- forecast(met_arima2_train, h=120, level=c(0.9,0.95))



# Convert forecasts to data frames
GLB_msts_arima2_df <- cbind( as.data.frame(met_arima2_train_pridect))  # Creating a data frame
GLB_msts_arima2_df$Date <- met_test$Date
summary(GLB_msts_arima2_df)

##plot1 overvall----
plot(MET3$Date,MET3$Median,xlab = 'Year', ylab = "Median",type='l',main ="Forcasts for Median 2007-2017 (MET)")
lines(GLB_msts_arima2_df$Date,GLB_msts_arima2_df$`Point Forecast`,col="red")

##average temp in 2007---14.6675
Amstrong<- data.frame(cbind("Date"= met_test$Date,"Median"=rep(14.4917,120)))

met_fit1 <- meanf(met_msts_train,h=120)
met_fit2 <- rwf(met_msts_train,h=120)
met_fit3 <- snaive(met_msts_train,h=120)

met_fit1_df <- cbind( as.data.frame(met_fit1))  # Creating a data frame
met_fit1_df$Date <- met_test$Date
met_fit2_df <- cbind( as.data.frame(met_fit2))  # Creating a data frame
met_fit2_df$Date <- met_test$Date
met_fit3_df <- cbind( as.data.frame(met_fit3))  # Creating a data frame
met_fit3_df$Date <- met_test$Date

##plot2 actual and predict data----

plot(met_test$Date, met_test$Median,type='l',xlab="Year", ylab="Temp",col="Black",main ="Predictions in different models(MET)",ylim=c(13.8,15.8))
lines(Amstrong$Date, Amstrong$Median,col="orange")
lines(GLB_msts_arima2_df$Date, GLB_msts_arima2_df$`Point Forecast`,col="red")
lines(met_fit1_df$Date, met_fit1_df$`Point Forecast`,col="purple")
lines(met_fit2_df$Date, met_fit2_df$`Point Forecast`,col="blue")
lines(met_fit3_df$Date, met_fit3_df$`Point Forecast`,col="pink")
legend("topleft", legend=c("Actual", "ARIMA(3,1,1)(0,1,0)[120]","Amstrong(Year)" ,"Average method","Naïve method(Month)","Seasonal naïve method"),
       col=c("black", "red","orange","purple","blue","pink"), lty=1:2, cex=0.8)

##plot3 overvall----
plot(MET3$Date,MET3$Median,xlab = 'Year', ylab = "Median",type='l',main ="Forcasts for Median 2007-2017(MET)")
lines(GLB_msts_arima2_df$Date, GLB_msts_arima2_df$`Point Forecast`,col="red")
lines(Amstrong$Date, Amstrong$Median,col="orange")
lines(met_fit1_df$Date, met_fit1_df$`Point Forecast`,col="purple")
lines(met_fit2_df$Date, met_fit2_df$`Point Forecast`,col="blue")
lines(met_fit3_df$Date, met_fit3_df$`Point Forecast`,col="pink")
legend("topleft", legend=c("Actual", "ARIMA(3,1,1)(0,1,0)[120]","Amstrong(Year)" ,"Average method","Naïve method(Month)","Seasonal naïve method"),
       col=c("black", "red","orange","purple","blue","pink"), lty=1:2, cex=0.8)

accuracy(GLB_msts_arima2_df$`Point Forecast`,met_test$Median)
#ME      RMSE       MAE        MPE     MAPE
#Test set -0.07739087 0.2058409 0.1545812 -0.5426347 1.064496
accuracy(Amstrong$Median,met_test$Median)
#               ME      RMSE      MAE       MPE      MAPE
#Test set 0.077475 0.1833835 0.137005 0.5189129 0.9343623
accuracy(met_fit1_df$`Point Forecast`,met_test$Median)
#ME      RMSE       MAE      MPE     MAPE
#Test set 0.7069108 0.7261887 0.7069108 4.839797 4.839797
accuracy(met_fit2_df$`Point Forecast`,met_test$Median)
#ME      RMSE       MAE      MPE     MAPE
#Test set 0.228175 0.2822959 0.2366583 1.553422 1.613015
accuracy(met_fit3_df$`Point Forecast`,met_test$Median)
#ME      RMSE      MAE       MPE     MAPE
#Test set 0.1110583 0.2328062 0.189375 0.7499246 1.295677

library(Metrics)
#Mean absolute scaled error:When comparing forecasting methods, the method with the lowest MASE is the preferred method.
mase(met_test$Median, GLB_msts_arima2_df$`Point Forecast`) #2.159564
mase(met_test$Median, Amstrong$Median) #1.914017
mase(met_test$Median, met_fit1_df$`Point Forecast`) #9.875837
mase(met_test$Median, met_fit2_df$`Point Forecast`)#3.306215
mase(met_test$Median, met_fit3_df$`Point Forecast`)#2.645647



#RMSE

e <- tsCV(nasa_msts_test, rwf, drift=TRUE, h=120)
sqrt(mean(e^2, na.rm=TRUE))
#0.3032744
sqrt(mean(residuals(rwf(nasa_msts_train, drift=TRUE))^2, na.rm=TRUE))
#0.1207451



far2 <- function(x, h){forecast(Arima(x, order=c(3,1,1)), h=h)}
e1 <- tsCV(nasa_msts_test, far2, h=120)
mse1 <- colMeans(e1^2, na.rm = T)

# Plot the MSE values against the forecast horizon
visual1<-data.frame(h = 1:120, MSE = mse1) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()+labs(title="MSE plot for Arima method")

e2 <- tsCV(nasa_msts_test, naive, h=120)
# Compute the MSE values and remove missing values
mse2 <- colMeans(e2^2, na.rm = T)
# Plot the MSE values against the forecast horizon
visual2<-data.frame(h = 1:120, MSE = mse2) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()+labs(title="MSE plot for naive method")


visual1<-data.frame(h = 1:120, MSE = mse1)
visual2<-data.frame(h = 1:120, MSE = mse2)
plot(visual1$h,visual1$MSE,col="red",main="MSE plot for ARIMA & Naive (MET)")
lines(visual2$h,visual2$MSE,col="blue")
legend("topleft", legend=c("Arima", "Naive"),
       col=c("red","blue"), lty=1:2, cex=0.8)

##cross validation----------------

##  ARIMA ------------------------------------
met_2017 <- subset(MET3,subset=MET3$Date<='2017-12-01') #1656 

met_msts_2017 <- msts(met_2017$Median, seasonal.periods = c(12,120))
accuracy.arima=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:10)
{
  nTest <- 12*i
  nTrain <- length(met_msts_2017)- nTest -1
  train <- window(met_msts_2017, start=1, end=1+(nTrain)/(120))
  test <- window(met_msts_2017, start=1+(nTrain+1)/(120), end=1+(nTrain+12)/(120))
  trainlm <- Arima(met_msts_2017,order=c(3,1,1),seasonal=c(0,1,0))
  trainlmf <- forecast(trainlm,h=120)
  sp<- as.numeric(trainlmf$mean)
  
  cat("----------------------------------
      Data Partition",i,"
      Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
      Test Set includes 12 time periods. Observations", nTrain+1, "to", nTrain+12,"
      ")
  print(accuracy(sp,test))
  #  print(residauto)
  accuracy.arima<-rbind(accuracy.arima,accuracy(sp,test)[1,5])
  #print(sp$model)
}

accuracy.arima<-accuracy.arima[-1]


mean(accuracy.arima) #1.244351
sd(accuracy.arima) #0.3675746

##  Amstrong ------------------------------------

accuracy.Amstrong=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:10)
{
  nTest <- 12*i
  nTrain <- length(met_msts_2017)- nTest -1
  train <- window(met_msts_2017, start=1, end=1+(nTrain)/(120))
  test <- window(met_msts_2017, start=1+(nTrain+1)/(120), end=1+(nTrain+12)/(120))
  sp<- as.numeric(Amstrong$Median)
  
  cat("----------------------------------
      Data Partition",i,"
      Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
      Test Set includes 12 time periods. Observations", nTrain+1, "to", nTrain+12,"
      ")
  print(accuracy(sp,test))
  #  print(residauto)
  accuracy.Amstrong<-rbind(accuracy.Amstrong,accuracy(sp,test)[1,5])
  #print(sp$model)
}

accuracy.Amstrong<-accuracy.Amstrong[-1]

mean(accuracy.Amstrong) # 1.008252
sd(accuracy.Amstrong) #0.6433732

write.csv(met_arima2_train_pridect, "met_pred_Q6.csv", row.names = FALSE)
write.csv(met_fit1, "met_pred2_Q6.csv",row.names = FALSE)
write.csv(met_fit2, "met_pred3_Q6.csv",row.names = FALSE)
write.csv(met_fit3, "met_pred4_Q6.csv",row.names = FALSE)


# Q7---------------------------------------
############ NASA ############
##1999 10 years interval##-----------------------------------------------------------------------
train.nasa.1999 <- subset(CLB3,subset=CLB3$Date<'1999-01-01') 
tail(train.nasa.1999)
#train.nasa.naive.1999 <- aggregate.ts(train.nasa.1999, nfrequency =1, mean)
test.nasa.1999  <- subset(CLB3,subset=CLB3$Date>='1999-01-01')
test.nasa.1999  <- subset(test.nasa.1999,subset=test.nasa.1999$Date<'2009-01-01')
tail(test.nasa.1999)
head(test.nasa.1999)
#test.nasa.naive.1999 <- aggregate.ts(test.nasa.1999, nfrequency =1, mean)

nasa_msts_train <- msts(train.nasa.1999$Temperature, seasonal.periods = c(12,120))
nasa.q6.1999 <- Arima(nasa_msts_train,order=c(3,1,3), seasonal=c(0,1,0))
prediction.nasa.1999 <- forecast(nasa.q6.1999,h=120)
plot(prediction.nasa.1999)
prediction.nasa.1999

# Convert forecasts to data frames
GLB_msts_arima2_df_10 <- cbind( as.data.frame(prediction.nasa.1999))  # Creating a data frame
#GLB_msts_arima2_df_10$Date <- test.nasa.1999$Date
summary(GLB_msts_arima2_df_10)
GLB_msts_arima2_df_10$Date <- test.nasa.1999$Date
nasa_model1 <- accuracy(GLB_msts_arima2_df_10$`Point Forecast`,test.nasa.1999$Temperature)

#Testing LBox
tsdisplay(residuals(nasa.q6.1999))
lb <- Box.test(residuals(nasa.q6.1999), lag = 24, type = "Ljung-Box")
lb

#NAIVE Model
nasa_fit1 <- rwf(nasa_msts_train,h=120)
nasa_fit1_df <- cbind( as.data.frame(nasa_fit1))
nasa_naive_model1 <- accuracy(nasa_fit1_df$`Point Forecast`,test.nasa.1999$Temperature)
plot(nasa_fit1)

accuracy_table1 <- rbind(nasa_model1,nasa_naive_model1)
knitr::kable(accuracy_table1, row.names = TRUE)

par(mfrow=c(2,1))
plot(prediction.nasa.1999)
plot(nasa_fit1)

##1999 20 years interval## -----------------------------------------------------------------
train.nasa.1999.2 <- subset(CLB3,subset=CLB3$Date<'1999-01-01')
tail(train.nasa.1999.2)
test.nasa.2 <- subset(CLB3,subset=CLB3$Date>='1999-01-01')
head(test.nasa.2)
test.nasa.2 <- subset(test.nasa.2,subset=test.nasa.2$Date<='2018-12-01')
tail(test.nasa.2)

#ARIMA
nasa_msts_train.2 <- msts(train.nasa.1999.2$Temperature, seasonal.periods = c(12,240))
nasa.q6.1999.2 <- Arima(nasa_msts_train.2,order=c(3,1,3), seasonal=c(0,1,0))
prediction.nasa.1999.2 <- forecast(nasa.q6.1999.2,h=240)
plot(prediction.nasa.1999.2)
prediction.nasa.1999.2

# Convert forecasts to data frames
GLB_msts_arima2_df_20 <- cbind( as.data.frame(prediction.nasa.1999.2))  # Creating a data frame
#GLB_msts_arima2_df_20$Date <- nasa_test$Date
summary(GLB_msts_arima2_df_20.2)
nasa_model <- accuracy(GLB_msts_arima2_df_20.2$`Point Forecast`,test.nasa.2$Temperature)
###
######           ME    RMSE      MAE        MPE     MAPE
#Test set -0.02027886 0.21752 0.178061 -0.1512981 1.213599

tsdisplay(residuals(nasa.q6.1999.2))
lb <- Box.test(residuals(nasa.q6.1999.2), lag = 24, type = "Ljung-Box")
lb

#NAIVE Model
nasa_fit2 <- rwf(nasa_msts_train.2,h=240)
nasa_fit2_df <- cbind( as.data.frame(nasa_fit2))
nasa_naive_model <- accuracy(nasa_fit2_df$`Point Forecast`,test.nasa.2$Temperature)
### 
#             ME     RMSE    MAE       MPE     MAPE
#Test set 0.12025 0.2261876 0.1745 0.8028722 1.180365


accuracy_table <- rbind(nasa_model,nasa_naive_model)
knitr::kable(accuracy_table, row.names = TRUE)

par(mfrow=c(2,1))
plot(prediction.nasa.1999.2)
plot(nasa_fit2)

par(mfrow=c(2,2))
plot(prediction.nasa.1999)
plot(nasa_fit1)
plot(prediction.nasa.1999.2)
plot(nasa_fit2)

##### UK MET DATA ##### --------------------------------------------------
##1999 10 years interval## ---------------------------------------------------------------
train.met.1999 <- subset(CLB3,subset=CLB3$Date<'1999-01-01') 
tail(train.met.1999)
test.met.1999  <- subset(CLB3,subset=CLB3$Date>='1999-01-01')
test.met.1999  <- subset(test.met.1999,subset=test.met.1999$Date<'2009-01-01')
tail(test.met.1999)
head(test.met.1999)
train.uk.met.1999 <- aggregate.ts(train.met.1999, nfrequency =1, mean)

####
met_msts_train <- msts(train.met.1999$Temparature, seasonal.periods = c(12,120))
met.q6.1999 <- Arima(met_msts_train,order=c(3,1,3), seasonal=c(0,1,0))
prediction.met.1999 <- forecast(met.q6.1999,h=120)
plot(prediction.met.1999)
prediction.met.1999

####
# Convert forecasts to data frames
met_msts_arima_df <- cbind( as.data.frame(prediction.met.1999))  # Creating a data frame
#GLB_msts_arima2_df$Date <- nasa_test$Date
summary(met_msts_arima_df)
met_model1 <- accuracy(met_msts_arima_df$`Point Forecast`,test.met.1999$Temparature)
accuracy(met_msts_arima_df$`Point Forecast`,test.met.1999$Temparature)

####
#NAIVE Model
met_fit <- rwf(met_msts_train,h=120)
met_fit1_df <- cbind( as.data.frame(met_fit))
met_naive_model1 <- accuracy(met_fit1_df$`Point Forecast`,test.met.1999$Temparature)

accuracy_table <- rbind(met_model1 ,met_naive_model1)
knitr::kable(accuracy_table, row.names = TRUE)


##1999 20 years interval## ------------------------------------------------------------------------------
train.met.1999.2 <- subset(CLB3,subset=CLB3$Date<'1999-01-01')
tail(train.met.1999.2)
test.met.2 <- subset(CLB3,subset=CLB3$Date>='1999-01-01')
head(test.met.2)
test.met.2 <- subset(test.met.2,subset=test.met.2$Date<='2018-12-01')
tail(test.met.2)

#ARIMA
met_msts_train.2 <- msts(train.met.1999.2$Temparature, seasonal.periods = c(12,240))
met.q6.1999.2 <- Arima(met_msts_train.2,order=c(3,1,3), seasonal=c(0,1,0))
checkresiduals(met.q6.1999.2)
prediction.met.1999.2 <- forecast(met.q6.1999.2,h=240)
plot(prediction.met.1999.2)
prediction.met.1999.2

# Convert forecasts to data frames
met_msts_arima2_df.2 <- cbind( as.data.frame(prediction.met.1999.2))  # Creating a data frame
#GLB_msts_arima2_df$Date <- nasa_test$Date
summary(met_msts_arima2_df.2)
met_msts_arima2_df.2$Date <- test.met.2$Date
met_model <- accuracy(met_msts_arima2_df.2$`Point Forecast`,test.met.2$Temparature)
###
##plot1 overvall----
plot(met1$Date,met1$Temparature,xlab = 'Year', ylab = "Temperature",type='l',main ="Forcasts for Temperature 1999-2018")
lines(met_msts_arima2_df.2$Date,met_msts_arima2_df.2$`Point Forecast`,col="red")
#
#NAIVE Model
met_fit2 <- rwf(met_msts_train.2,h=240)
met_fit2_df <- cbind( as.data.frame(met_fit2))
met_naive_model <- accuracy(met_fit2_df$`Point Forecast`,test.met.2$Temparature)



accuracy_table_2 <- rbind(met_model,met_naive_model)
knitr::kable(accuracy_table_2, row.names = TRUE)

par(mfrow=c(2,2))
plot(prediction.met.1999)
plot(met_fit)
plot(prediction.met.1999.2)
plot(met_fit2)
 