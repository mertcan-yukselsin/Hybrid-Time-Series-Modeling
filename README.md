# Hybrid-Time-Series-Modeling
This Repository is about a new approach related to Hybrid Time Series modeling.

This repository introduces a new univariate time series modeling approach for the data Climate Change Time Series Data. The new method uses nine different models while constructing the final model. Throughout the analyses, R-Studio is used.

In Hybrid-Modeling.html file, tutorial codes are given accordingly. In that file, Unemployment Rate Data is used.

In hybrid-approach.pdf file, a paper is written so that the method is examined in detail.

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
