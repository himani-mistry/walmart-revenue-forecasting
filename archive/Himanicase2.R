#Himani case 2 assignment

#Setup
install.packages("forecast")
library(forecast)

# Load data
setwd("/Users/himanimistry/Downloads")
walmart.data <- read.csv("673_case2.csv")

head(walmart.data)
tail(walmart.data)

# 1a  
revenue.ts <- ts(walmart.data$Revenue,
                 start = c(2006, 1),
                 end = c(2025, 3),
                 frequency = 4)
revenue.ts

# 1b
plot(revenue.ts, 
     xlab = "Time", ylab = "Revenue (in millions)", 
     ylim = c(50000, 200000), xaxt = 'n',
     main = "Walmart's historical quarterly revenue")

axis(1, at = seq(2006, 2025, 1), labels = format(seq(2006, 2025, 1)))

# 2a
nValid <- 19
nTrain <- length(revenue.ts) - nValid

train.ts <- window(revenue.ts, end = time(revenue.ts)[nTrain])
valid.ts <- window(revenue.ts, start = time(revenue.ts)[nTrain + 1])

# 2b 

#Regression model w seasonality
model_season <- tslm(train.ts ~ season)
summary(model_season)

#Regression model w linear trend and seasonality
model_linear <- tslm(train.ts ~ trend + season)
summary(model_linear)

#Regression model w quadratic trend and seasonality
model_quad <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(model_quad)

#Forecasts
model1_pred <- forecast(model_season, h = nValid, level = 0)
model1_pred

model2_pred <- forecast(model_linear, h = nValid, level = 0)
model2_pred

model3_pred <- forecast(model_quad, h = nValid, level = 0)
model3_pred

# 2c
#Accuracy check
round(accuracy(model1_pred$mean, valid.ts), 3)
round(accuracy(model2_pred$mean, valid.ts), 3)
round(accuracy(model3_pred$mean, valid.ts), 3)

# 3a
model_linear_full <- tslm(revenue.ts ~ trend + season)
summary(model_linear_full)

model_quad_full <- tslm(revenue.ts ~ trend + I(trend^2) + season)
summary(model_quad_full)

#Forecasts
forecast_linear <- forecast(model_linear_full, h = 9, level = 0)
forecast_linear

forecast_quad   <- forecast(model_quad_full, h = 9, level = 0)
forecast_quad

future9.df <- round(data.frame( Linear_Forecast = forecast_linear$mean, Quadratic_Forecast = forecast_quad$mean), 2)
future9.df


# 3b
naive_full <- naive(revenue.ts)
snaive_full <- snaive(revenue.ts)

round(accuracy(naive_full$fitted, revenue.ts), 3)
round(accuracy(snaive_full$fitted, revenue.ts), 3)
round(accuracy(model_linear_full$fitted, revenue.ts), 3)
round(accuracy(model_quad_full$fitted, revenue.ts), 3)



