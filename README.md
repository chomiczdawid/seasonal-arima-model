## Seasonal ARIMA model
This repository contains a paper on application of seasonal ARIMA model for forecasting GDP of United Kingdom. To preview the paper, click on the file `paper.pdf`. PDF was generated using R Markdown. For clean R code click on the file `code.R`.

### Analytical process carried out:
- initial analysis using descriptive statistics
- determining time series stationarity with ADF and HEGY tests
- estimating seasonal ARIMA models and chosing the best one based on AIC
- interpretation of error measures
- CUSUM and Chow stability tests

## Used technology
- [R version 4.1.3](https://cran.r-project.org/src/base/R-4/)
- [RStudio](https://www.rstudio.com/)

## Used libraries
```r
library(readxl)     # for xlsx loading
library(tidyverse)  # vide range analytical tools
library(zoo)        # conversion to time series
library(urca)       # ADF test with restrictions
library(car)        # Linear restrictions test
library(moments)    # additional descriptive statistics 
library(forecast)   # forecasting
library(strucchange)# CUSUM tests
```
