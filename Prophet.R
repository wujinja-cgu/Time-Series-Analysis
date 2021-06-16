## Load packages
library(prophet)
library(dplyr)
library(plyr)
library(hydroGOF)
library(plotly)
library(forecast)

##########################################
########## Data pre-processing ###########
##########################################
### Import Data
setwd("C:/Users/Chin Chieh Wu/Desktop/CGU outcome research/Example of Time series analysis/prophet")
mydata=read.csv("time series data.csv")
mydata=as.data.frame(mydata)
head(mydata)
nrow(mydata)

### Re-scale the date format
colnames(mydata)=c("ds","y")
mydata$ds=seq(as.Date('2007-10-09'),by='days',length=nrow(mydata))


#########################################
##########  Model derivation  ###########
#########################################

### Split data into training and testing sets
training=mydata[1:2641,]    # years 2007-2014 as the training set
testing=mydata[2642:2996,]  # year 2015 as the testing set
testing[1:10,]

### Modeling
modelfit=prophet(df=training,
                 growth="linear",
                 changepoints=NULL,
                 n.changepoints=25,
                 yearly.seasonality=TRUE,
                 weekly.seasonality=TRUE,
                 daily.seasonality=TRUE,
                 holidays=NULL,
                 seasonality.prior.scale=10,
                 changepoint.prior.scale=2.8,
                 holidays.prior.scale=10,
                 mcmc.samples=100,
                 interval.width=0.8,
                 uncertainty.samples=1000,
                 fit=TRUE)

future=make_future_dataframe(modelfit,periods=355)
future$cap=450
tail(future)

### Make prediction
forecast=predict(modelfit,future)
pred_train=forecast[1:2641,c('ds','yhat','yhat_lower','yhat_upper')]
pred_train=as.data.frame(pred_train)
pred_test=forecast[2642:2996,c('ds','yhat','yhat_lower','yhat_upper')]
pred_test=as.data.frame(pred_test)
preddata_train=cbind(training[,"y"],pred_train[,'yhat'])
preddata_test=cbind(testing[,"y"],pred_test[,'yhat'])

### Calculate the error of prediction

## error
error_train=c(preddata_train[,1])-c(preddata_train[,2])
error_test=c(preddata_test[,1])-c(preddata_test[,2])

## MAPE
MAPE_train=mean(abs(c(error_train)/c(preddata_train[,1])))
MAPE_test=mean(abs(c(error_test)/c(preddata_test[,1])))

## MAE
MAE_train=mean(abs(c(error_train)))
MAE_test=mean(abs(c(error_test)))

## RMSE
RMSE_train=rmse(preddata_train[,1],preddata_train[,2])
RMSE_test=rmse(preddata_test[,1],preddata_test[,2])

## Print the results
results=list("MAPE of training"=MAPE_train,
             "MAPE of testing"=MAPE_test,
             "MAE of training"=MAE_train,
             "MAE of testing"=MAE_test,
             "RMSE of training"=RMSE_train,
             "RMSE of testing"=RMSE_test)
print(results)

## Time series plot
windows()
plot(modelfit,forecast)
