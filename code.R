# libraries
library(readxl)     # for xlsx loading
library(tidyverse)  # vide range analytical tools
library(zoo)        # conversion to time series
library(urca)       # ADF test with restrictions
library(car)        # Linear restrictions test
library(moments)    # additional descriptive statistics 
library(forecast)   # forecasting
library(strucchange)# CUSUM tests

# data loading
df_raw <- read_excel("gdp_uk.xlsx")
df <- df_raw[101:267,]

# Initial analysis
summary(df$gdp)
stats <- cbind(sd(df$gdp),(sd(df$gdp) / mean(df$gdp) * 100),skewness(df$gdp),kurtosis(df$gdp))
colnames(stats) <- c("SD","CV","Skewness","Kurtosis")
rownames(stats) <- " "

# GDP Chart
fmt <- "%Y Q%q"
df$date <- as.yearqtr(df$date, format = fmt)
ggplot(df) +
  geom_line(aes(x = date, y = gdp), color = 'red') +
  scale_x_yearqtr(format = fmt) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "GDP",
       x = "Time") +
  theme_bw()

# Function to summarize the ADF variants
adf_summary <- function(x) {
  adf_none  <-  ur.df(x, lags = 8, type='none', selectlags = c("AIC"))
  adf_drift <-  ur.df(x, lags = 8, type='drift', selectlags = c("AIC"))
  adf_trend <-  ur.df(x, lags = 8, type='trend', selectlags = c("AIC"))
  print("ADF without trend and constant")
  print(cbind(t(adf_none@teststat), adf_none@cval))
  print("ADF with constant")
  print(cbind(t(adf_drift@teststat), adf_drift@cval))
  print("ADF with constant and trend")
  print(cbind(t(adf_trend@teststat), adf_trend@cval))
}

# For unchanged data
adf_summary(df$gdp)

# For first seasonal differences
diff1 <- as.vector(diff(df$gdp, lag = 4))
adf_summary(diff1)

# HEGY
# the lags use the full range of data (df_raw) so that there are no null values
y1 <- head(df$gdp + df_raw[100:267,]$gdp + df_raw[99:267,]$gdp + df_raw[98:267,]$gdp,-3)
y2 <- -(head(df$gdp - df_raw[100:267,]$gdp + df_raw[99:267,]$gdp - df_raw[98:267,]$gdp,-3))
y3 <- head(df$gdp - df_raw[99:267,]$gdp,-2)
y4 <- head(df$gdp - df_raw[97:267,]$gdp,-4)

y1_1 <- lag(y1, n = 1)[-1:-4]
y2_1 <- lag(y2, n = 1)[-1:-4]
y3_1 <- lag(y3, n = 1)[-1:-4]
y3_2 <- lag(y3, n = 2)[-1:-4]
y4_1 <- lag(y4, n = 1)[-1:-4]
y4_2 <- lag(y4, n = 2)[-1:-4]
y4_3 <- lag(y4, n = 3)[-1:-4]
y4_4 <- lag(y4, n = 4)[-1:-4]

lm1 <- lm(y4[-1:-4] ~ y1_1 + y2_1 + y3_1 + y3_2 + y4_1 + y4_2 + y4_3 + y4_4)
summary(lm1)

# Linear restrictions test
linearHypothesis(lm1, c("y3_1 = 0","y3_2 = 0"))

# SARIMA model
# Conversion to time series object
dfts <- ts(df$gdp, start = c(1980,1), end = c(2021,3), frequency = 4)

# Models estimation
m1 <- Arima(dfts, order = c(2,1,2), seasonal = c(2,1,2))
m2 <- Arima(dfts, order = c(2,1,2), seasonal = c(1,1,2))
m3 <- Arima(dfts, order = c(2,1,2), seasonal = c(1,1,1))
m4 <- Arima(dfts, order = c(2,1,1), seasonal = c(1,1,1))
m5 <- Arima(dfts, order = c(1,1,1), seasonal = c(1,1,1))
m6 <- Arima(dfts, order = c(1,1,1), seasonal = c(0,1,1))
m7 <- Arima(dfts, order = c(0,1,1), seasonal = c(0,1,1))

# Model comparison - the Akaike criterion
wnk <- cbind(2,1,2,2,1,2,m1$aic)
wnk <- rbind(wnk, c(2,1,2,1,1,2,m2$aic))
wnk <- rbind(wnk, c(2,1,2,1,1,1,m3$aic))
wnk <- rbind(wnk, c(2,1,1,1,1,1,m4$aic))
wnk <- rbind(wnk, c(1,1,1,1,1,1,m5$aic))
wnk <- rbind(wnk, c(1,1,1,0,1,1,m6$aic))
wnk <- rbind(wnk, c(0,1,1,0,1,1,m7$aic))
colnames(wnk) <- c("p","d","q","P","D","Q","AIC")
rownames(wnk) <- c("M1","M2","M3","M4","M5","M6","M7")

# Model 6 and forecast
fc <- forecast(m6, h=1, level = 95)
summary(fc)
fsd <- (fc$upper[,1] - fc$lower[,1]) / (2 * qnorm(.5 + fc$level[1] / 200))
cat("Standard error: ", fsd)

# Forecast plot
autoplot(as.zoo(m6$x), geom = "line") + 
  autolayer(m6$x, series = "PKB") +
  autolayer(fitted(m6), series = "prognoza") + 
  autolayer(forecast(m6,h=1), series = "przedzia³ ufnoœci") + 
  scale_x_yearqtr() + 
  scale_y_continuous(labels = scales::comma) +
  labs(y = "",x = "") +
  theme_bw()

# CUSUM
ma <- as.vector(m6$residuals)
ma <- ma[-1]
ma_1 <- lag(ma, n = 1)[-1:-4]
ma_4 <- lag(ma, n = 4)[-1:-4]

d1gdp <- df$gdp - lag(df$gdp, n = 1)
d1gdp <- d1gdp[-1]

d1s1gdp <- d1gdp - lag(d1gdp, n = 4)
d1s1gdp <- d1s1gdp[-1:-4]

cusum <- efp(d1s1gdp ~ ma_1 + ma_4, type = "Rec-CUSUM")
plot(cusum, main = "")

# CUSUM-SQ
# squares of recursive residuals
rr2 <- recresid(d1s1gdp ~ ma_1 + ma_4)^2
# CUSUM process
sr <- ts(cumsum(c(0, rr2))/sum(rr2), freq = 4)
# confidence intervals
border <- ts(seq(0, 1, length = length(sr)), start = start(sr), freq = 4)
# plot
plot(sr, xaxs = "i", yaxs = "i", main = NULL, type = "o", col = 3)
lines(border, lty = 3, lwd = 2)
lines(0.2 + border, col = 2, lwd = 2)
lines(- 0.2 + border, col = 2, lwd = 2)

# Chow test
sctest(d1s1gdp ~ ma_1 + ma_4, type = "Chow")

# QLR test
qlr <- Fstats(d1s1gdp ~ ma_1 + ma_4)
plot(qlr,alpha=0.05)