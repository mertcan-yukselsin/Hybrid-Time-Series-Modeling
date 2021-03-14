# Hybrid-Time-Series-Modeling
This Repository is about a new approach related to Hybrid Time Series modeling.

This repository introduces a new univariate time series modeling approach for the data Climate Change Time Series Data. The new method uses nine different models while constructing the final model. Throughout the analyses, R-Studio is used.

In Hybrid-Modeling.html file, tutorial codes are given accordingly. In that file, Unemployment Rate Data is used.

In hybrid-approach.pdf file, a paper is written so that the method is examined in detail.


---
title: "Hybrid Models Tutorial"
author: "Mertcan Yükselsin"
date: "`r Sys.Date()`"
output:
  rmdformats::robobook:
    highlight: kate
---

# Necessary Libraries

```{r,warning=FALSE,message=FALSE}
library(tidyverse)
library(forecast)
library(tseries)
library(prophet)
library(lubridate)
library(bsts)
```




# Introduction

In this tutorial, hybrid models are used to model a univariate time series. In this case, data for Climate Change CH4 starting from 1983 May to 2009 monthly data is used.


# Getting Data

```{r}
data=read.csv("climate_change.csv")
data=data$CH4
head(data)
tsdata=ts(data,start=c(1983,5),frequency = 12)
```







# Splitting Data into Two Parts

```{r}
train=window(tsdata,end=c(2007,12))
test=window(tsdata,start=2008)
train_date_start="1983/05/01"
train_date_end="2007/12/01"
test_date_start="2008/01/01"
test_date_end="2008/12/01"
```


```{r}
autoplot(train,main="Train Data",col="red")+theme_minimal()
```






# Time Series Decomposition


First of all, time series is divided into 3 parts, namely seasonal, trend and remainder component by STL.

```{r}
decomposed=stl(train ,s.window = "periodic")
seasonal=decomposed$time.series[,1]
trend=decomposed$time.series[,2]
remainder=decomposed$time.series[,3]
```




```{r}
autoplot(decomposed)
```



# Modeling





In this section, in total, 8 models are used **1: ARIMA, 2: ETS, 3:TBATS , 4: NNETAR , 5: PROPHET , 6: Bayesian , 7: ARFIMA , 8: STLM , 9: HOLT** . Note that here automated models are used for each model (one can also look at each component seperately to suggest manual models and try its performance).

```{r}
set.seed(1234)
#For Seasonal Component
seasonal_arima=auto.arima(seasonal)
seasonal_ets=ets(seasonal)
seasonal_tbats=tbats(seasonal)
seasonal_nnar=nnetar(seasonal)
seasonal_arfima=arfima(seasonal)
seasonal_stlm=stlm(seasonal)
seasonal_holt=holt(seasonal,h=length(test))
#For Trend Component
trend_arima=auto.arima(trend)
trend_ets=ets(trend)
trend_tbats=tbats(trend)
trend_nnar=nnetar(trend)
trend_arfima=arfima(trend)
trend_stlm=stlm(trend)
trend_holt=holt(trend,h=length(test))
#For Remainder Component
remainder_arima=auto.arima(remainder)
remainder_ets=ets(remainder)
remainder_tbats=tbats(remainder)
remainder_nnar=nnetar(remainder)
remainder_arfima=arfima(remainder)
remainder_stlm=stlm(remainder)
remainder_holt=holt(remainder,h=length(test))
```





```{r,message=FALSE}
#for prophet
ds=c(seq(as.Date(train_date_start),as.Date(train_date_end),by="month")) 
test_ds<-c(seq(as.Date(test_date_start),as.Date(test_date_end),by="month")) 
#for seasonal component
df_prophet_seasonal=data.frame(ds,y=as.numeric(seasonal)) 
model_prophet_seasonal <- prophet(df_prophet_seasonal) 
future_seasonal<-make_future_dataframe(model_prophet_seasonal,periods = length(test),freq = "month") 
forecast_seasonal <- predict(model_prophet_seasonal, future_seasonal)
prophet_fitted_seasonal= forecast_seasonal$yhat[1:length(train)]
prophet_forecast_seasonal=tail(forecast_seasonal$yhat,length(test))
#for trend component
df_prophet_trend=data.frame(ds,y=as.numeric(trend)) 
model_prophet_trend <- prophet(df_prophet_trend) 
future_trend<-make_future_dataframe(model_prophet_trend,periods = length(test),freq = "month") 
forecast_trend <- predict(model_prophet_trend, future_trend)
prophet_fitted_trend= forecast_trend$yhat[1:length(train)]
prophet_forecast_trend=tail(forecast_trend$yhat,length(test))
#for remainder component
df_prophet_remainder=data.frame(ds,y=as.numeric(remainder)) 
model_prophet_remainder <- prophet(df_prophet_remainder) 
future_remainder<-make_future_dataframe(model_prophet_remainder,periods = length(test),freq = "month") 
forecast_remainder <- predict(model_prophet_remainder, future_remainder)
prophet_fitted_remainder= forecast_remainder$yhat[1:length(train)]
prophet_forecast_remainder=tail(forecast_remainder$yhat,length(test))
```




```{r}
#seasonal
ss <- AddLocalLinearTrend(list(), seasonal)
ss <- AddSeasonal(ss, seasonal, nseasons = 12)
bsts.model <- bsts(seasonal, state.specification = ss, niter = 500, ping=0, seed=2016)
burn <- SuggestBurn(0.1, bsts.model)
bayes_seasonal_f <- as.numeric(predict.bsts(bsts.model, horizon = length(test), burn = burn, quantiles = c(.025, .975))$mean)
bayes_seasonal_fitted=as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),])+seasonal)
#trend
ss <- AddLocalLinearTrend(list(), trend)
ss <- AddSeasonal(ss, trend, nseasons = 12)
bsts.model <- bsts(trend, state.specification = ss, niter = 500, ping=0, seed=2016)
burn <- SuggestBurn(0.1, bsts.model)
bayes_trend_f <- as.numeric(predict.bsts(bsts.model, horizon = length(test), burn = burn, quantiles = c(.025, .975))$mean)
bayes_trend_fitted=as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),])+trend)
#remainder
ss <- AddLocalLinearTrend(list(), remainder)
ss <- AddSeasonal(ss, remainder, nseasons = 12)
bsts.model <- bsts(remainder, state.specification = ss, niter = 500, ping=0, seed=2016)
burn <- SuggestBurn(0.1, bsts.model)
bayes_remainder_f <- as.numeric(predict.bsts(bsts.model, horizon = length(test), burn = burn, quantiles = c(.025, .975))$mean)
bayes_remainder_fitted=as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),])+remainder)
```






```{r}
#Forecasting (Other models)
#For Seasonal Component
seasonal_arima_f=forecast(seasonal_arima,h=length(test))$mean
seasonal_ets_f=forecast(seasonal_ets,h=length(test))$mean
seasonal_tbats_f=forecast(seasonal_tbats,h=length(test))$mean
seasonal_nnar_f=forecast(seasonal_nnar,h=length(test))$mean
seasonal_arfima_f=forecast(seasonal_arfima,h=length(test))$mean
seasonal_stlm_f=forecast(seasonal_stlm,h=length(test))$mean
seasonal_holt_f=seasonal_holt$mean
#For Trend Component
trend_arima_f=forecast(trend_arima,h=length(test))$mean
trend_ets_f=forecast(trend_ets,h=length(test))$mean
trend_tbats_f=forecast(trend_tbats,h=length(test))$mean
trend_nnar_f=forecast(trend_nnar,h=length(test))$mean
trend_arfima_f=forecast(trend_arfima,h=length(test))$mean
trend_stlm_f=forecast(trend_stlm,h=length(test))$mean
trend_holt_f=trend_holt$mean
#For Remainder Component
remainder_arima_f=forecast(remainder_arima,h=length(test))$mean
remainder_ets_f=forecast(remainder_ets,h=length(test))$mean
remainder_tbats_f=forecast(remainder_tbats,h=length(test))$mean
remainder_nnar_f=forecast(remainder_nnar,h=length(test))$mean
remainder_arfima_f=forecast(remainder_arfima,h=length(test))$mean
remainder_stlm_f=forecast(remainder_stlm,h=length(test))$mean
remainder_holt_f=remainder_holt$mean
```



After getting fitted and forecasted values for each model, let's combine them into one dataframe for each component seperately.

```{r}
seasonal_df=data.frame(seasonal_arima_f,seasonal_ets_f,
                       seasonal_tbats_f,seasonal_nnar_f,prophet_forecast_seasonal,
                       bayes_seasonal_f,seasonal_arfima_f,seasonal_stlm_f,seasonal_holt_f)
trend_df=data.frame(trend_arima_f,trend_ets_f,trend_tbats_f,trend_nnar_f,prophet_forecast_trend,
                    bayes_trend_f,trend_arfima_f,trend_stlm_f,trend_holt_f)
remainder_df=data.frame(remainder_arima_f,
                        remainder_ets_f,remainder_tbats_f,remainder_nnar_f,
                        prophet_forecast_remainder,bayes_remainder_f,remainder_arfima_f,
                        remainder_stlm_f,remainder_holt_f)
```


```{r}
head(seasonal_df) #forecasts for seasonal component
```



```{r}
#1: ARIMA, 2: ETS, 3:TBATS  , 4: NNETAR , 5: Prophet , 6: Bayesian , 7: ARFIMA , 8: STLM , 9: HOLT
model_accuracy_hybrid=function(seasonal,trend,remainder,test){
  forecasts=seasonal_df[,seasonal]+trend_df[,trend]+remainder_df[,remainder]
  accuracy(forecasts,test)
}
```

In this model_accuracy_hybrid() function, I considered using their corresponding values for different values. For example, when seasonal=1, it represents the ARIMA model since it is the first column of that dataframe and so on.



```{r}
#For example, Fitting Seasonal part as ARIMA, trend as ARIMA and Remainder as ARIMA
model_accuracy_hybrid(seasonal=1,trend=1,remainder=1,test) 
```

```{r}
#Fitting an auto.arima model directly
accuracy(forecast(auto.arima(train),h=12)$mean,test)
```
Here, I considered using ARIMA model for each component separately and auto.arima function directly. As it is seen from the accuracy measures, mostly the hybrid models works better even we use ARIMA model for each component separately.



```{r}
forecasts=as.data.frame(matrix(ncol=7,nrow=9^3))
colnames(forecasts)=c("ME","RMSE","MAE","MPE","MAPE","ACF1","Theil's U")
a=1
for(i in 1:9){
  for(j in 1:9){
    for(k in 1:9){
      forecasts[a,]=model_accuracy_hybrid(seasonal=i,trend=j,remainder=k,test=test)[1,]
      rownames(forecasts)[a]=paste("Seasonal:",as.character(i),"Trend:",as.character(j),"Remainder:",as.character(k))
      a=a+1 
      
    }
  }
}
```


Here, rownames represent seasonal,trend,remainder accordingly,

where **1: ARIMA, 2: ETS, 3:TBATS , 4: NNETAR , 5: PROPHET , 6: Bayesian , 7: ARFIMA , 8: STLM , 9: HOLT** 


```{r}
head(forecasts,10)
```




Now, let's obtain different models for different accuracy measures.

```{r}
forecasts[which.min(forecasts$ME), ] #min ME
forecasts[which.min(forecasts$RMSE), ] #min RMSE
forecasts[which.min(forecasts$MAE), ] #min MAE
forecasts[which.min(forecasts$MPE), ] #min MPE
forecasts[which.min(forecasts$MAPE), ] #min MAPE
forecasts[which.min(forecasts$ACF1), ] #min ACF1
forecasts[which.min(forecasts$`Theil's U`), ] #min Theil's U
```


It is seen that, using the model 5-6-3 seems useful.


```{r}
model_accuracy_hybrid(seasonal=5,trend=6,remainder=3,test)
```




Remark : **1: ARIMA, 2: ETS, 3:TBATS , 4: NNETAR , 5: PROPHET , 6: Bayesian , 7: ARFIMA , 8: STLM , 9: HOLT ** 

In this case, modeling Seasonal Component with Prophet, Trend Component with ARIMA and Remainder Component with TBATS works best choosing the lowest values among 7 accuracy measures.


# Plotting the Best Model



```{r}
fitted=ts(prophet_fitted_seasonal,start=start(train),frequency = 12)+ bayes_trend_fitted + remainder_tbats$fitted.values
forecasted_val=prophet_forecast_seasonal+bayes_trend_f+remainder_tbats_f
```




```{r}
accuracy_train=accuracy(fitted,train)
accuracy_test=accuracy(forecasted_val,test)
accuracy_bestmodel=rbind(accuracy_train,accuracy_test)
rownames(accuracy_bestmodel)=c("Train Set","Test Set")
accuracy_bestmodel
```


```{r}
prophet_forecast_seasonal
```


Let's plot the components separately.

```{r}
autoplot(decomposed$time.series[,1],ylab="Seasonal Component",series="Original",main="Seasonal Component")+
  autolayer(ts(prophet_fitted_seasonal,start=start(train),frequency = 12),series="Fitted")+
  autolayer(ts(prophet_forecast_seasonal,start=2008,frequency = 12),series="Forecasted")+theme_minimal()
```

```{r}
autoplot(decomposed$time.series[,2],ylab="Trend Component",series="Original",main="Trend Component")+
  autolayer(ts(bayes_trend_fitted,start=start(train),frequency = 12),series="Fitted")+
  autolayer(ts(bayes_trend_f,start=2008,frequency = 12),series="Forecasted")+theme_minimal()
```


```{r}
autoplot(decomposed$time.series[,3],ylab="Remainder Component",series="Original",main="Remainder Component")+
  autolayer(remainder_tbats$fitted.values,series="Fitted")+
  autolayer(remainder_tbats_f,series="Forecasted")+theme_minimal()
```



Finally, let's sum the components so that we would obtain the original series with test dataset.


```{r}
autoplot(fitted,series="Fitted",main = "Forecasts of the Hybrid Model",ylab="CH4")+
  autolayer(train,series="Train")+
  autolayer(test,series="Test")+
  autolayer(forecasted_val,series="Forecast")+ 
  geom_vline(xintercept=2008,col="red",size=1)+theme_minimal()
```




