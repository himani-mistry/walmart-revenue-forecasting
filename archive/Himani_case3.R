#Himani case 3 assignment

#Setup
install.packages("forecast")
library(forecast)

# Load data
setwd("/Users/himanimistry/Downloads")
walmart.data <- read.csv("673_case2.csv")

head(walmart.data)
tail(walmart.data)

revenue.ts <- ts(walmart.data$Revenue,
                 start = c(2006, 1),
                 end = c(2025, 3),
                 frequency = 4)
revenue.ts

nValid <- 19
nTrain <- length(revenue.ts) - nValid

train.ts <- window(revenue.ts, end = time(revenue.ts)[nTrain])
valid.ts <- window(revenue.ts, start = time(revenue.ts)[nTrain + 1])

# 1a
#Ar(1) model on entire historical data
ar1.full <- Arima(revenue.ts, order = c(1,0,0))
summary(ar1.full)

# 1b
#First differencing + ACF
revenue.diff1 <- diff(revenue.ts, lag = 1)

Acf(revenue.diff1, lag.max = 8,
    main = "ACF of First-Differenced Walmart Revenue")

# 2a
#Regression with quadratic trend + seasonality on training
reg.quad <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(reg.quad)

reg.pred <- forecast(reg.quad, h = nValid, level = 0)
reg.pred

# 2b 
#Residuals of regression model + ACF
reg.res <- residuals(reg.quad)

Acf(reg.res, lag.max = 8,
    main = "ACF of Regression Residuals")

# 2c
#Ar(1) model for regression residuals
ar1.res <- Arima(reg.res, order = c(1,0,0))
summary(ar1.res)

#residuals of residual model
ar1.res.resid <- residuals(ar1.res)

Acf(ar1.res.resid, lag.max = 8,
    main = "ACF of AR(1) Residuals")

# 2d
#two level forecast for validation period
#forecast residuals using Ar(1)
ar1.res.pred <- forecast(ar1.res, h = nValid, level = 0)
ar1.res.pred

#combined forecasts
combined.pred <- reg.pred$mean + ar1.res.pred$mean

#create report table
valid.df <- round(data.frame(
  Validation_Data = valid.ts,
  Regression_Forecast = reg.pred$mean,
  AR1_Residual_Forecast = ar1.res.pred$mean,
  Combined_Forecast = combined.pred
), 3)

valid.df

#accuracy of regression vs combined
round(accuracy(reg.pred$mean, valid.ts), 3)
round(accuracy(combined.pred, valid.ts), 3)

# 2e
#two level forecast entire dataset
reg.quad.full <- tslm(revenue.ts ~ trend + I(trend^2) + season)
summary(reg.quad.full)

#forecast 9 quarter ahead
reg.full.pred <- forecast(reg.quad.full, h = 9, level = 0)
reg.full.pred

#residuals on full data
reg.full.res <- residuals(reg.quad.full)

#Ar(1) on full data residuals
ar1.full.res <- Arima(reg.full.res, order = c(1,0,0))
summary(ar1.full.res)

#ACF of Ar(1) residuals
Acf(residuals(ar1.full.res), lag.max = 8,
    main = "ACF of AR(1) Residuals - Full Data")

#forecast 9 quarter ahead
ar1.full.pred <- forecast(ar1.full.res, h = 9, level = 0)
ar1.full.pred

#combined forecast
combined.full.pred <- reg.full.pred$mean + ar1.full.pred$mean

#future table
future9.df <- round(data.frame(
  Regression_Forecast = reg.full.pred$mean,
  AR1_Residual_Forecast = ar1.full.pred$mean,
  Combined_Forecast = combined.full.pred
), 3)

future9.df


# 3a
# Fixed ARIMA (1,1,1) (1,1,1) on training
arima.fixed <- Arima(train.ts,
                     order = c(1,1,1),
                     seasonal = c(1,1,1))
summary(arima.fixed)

#forecast validation
arima.fixed.pred <- forecast(arima.fixed, h = nValid, level = 0)
arima.fixed.pred

# 3b
# auto.arima() on training
arima.auto <- auto.arima(train.ts)
summary(arima.auto)

#forecast validation
arima.auto.pred <- forecast(arima.auto, h = nValid, level = 0)
arima.auto.pred

# 3c
#compare 2 model accuracy
round(accuracy(arima.fixed.pred$mean, valid.ts), 3)
round(accuracy(arima.auto.pred$mean, valid.ts), 3)


# 3d
#both ARIMA model on full dataset
arima.fixed.full <- Arima(revenue.ts,
                          order = c(1,1,1),
                          seasonal = c(1,1,1))
summary(arima.fixed.full)

arima.auto.full <- auto.arima(revenue.ts)
summary(arima.auto.full)


#forecast 9 quarter ahead
arima.fixed.full.pred <- forecast(arima.fixed.full, h = 9, level = 0)
arima.auto.full.pred <- forecast(arima.auto.full, h = 9, level = 0)

arima.fixed.full.pred
arima.auto.full.pred

# 3e
#compare 5 models: quadratic regression, two-level model, fixed ARIMA, auto ARIMA, seasonal naive

snaive.full <- snaive(revenue.ts)

round((accuracy(reg.quad.full$fitted, revenue.ts)), 3)
round((accuracy(reg.quad.full$fitted + fitted(ar1.full.res), revenue.ts)), 3)
round((accuracy(arima.fixed.full$fitted, revenue.ts)), 3)
round((accuracy(arima.auto.full$fitted, revenue.ts)), 3)
round((accuracy(snaive.full$fitted, revenue.ts)), 3)

