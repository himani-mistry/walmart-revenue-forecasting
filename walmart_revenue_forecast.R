# =============================================================================
# Walmart Quarterly Revenue — Forecasting Method Benchmark
# Himani Mistry
#
# Question:  How accurately can we forecast a large retailer's quarterly
#            revenue 1-2 years out, and which method should a planning team
#            actually trust?
#
# Method:    Build a ladder of forecasting models, then score each one on a
#            HELD-OUT validation window (not training fit) so the comparison
#            reflects real forecasting skill, not memorization.
#
# Headline:  On held-out data, auto-ARIMA forecast within ~4% (MAPE),
#            roughly half the error of the best regression model.
#            A two-level hybrid I engineered gave the tightest in-sample fit;
#            the residual diagnostics that motivated it are the real lesson.
# =============================================================================

library(forecast)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. Data
#    Walmart quarterly revenue, 2006 Q1 - 2025 Q3 (~79 quarters, $M).
# -----------------------------------------------------------------------------
walmart.data <- read.csv("673_case2.csv")
revenue.ts <- ts(walmart.data$Revenue,
                 start = c(2006, 1), end = c(2025, 3), frequency = 4)

# Strong upward trend + a hard Q4 (holiday) spike every year.
plot(revenue.ts, xlab = "Time", ylab = "Revenue ($M)",
     main = "Walmart quarterly revenue, 2006-2025")

# -----------------------------------------------------------------------------
# 2. Train / validation split
#    Last 19 quarters are held out. Every model below is judged ONLY on these
#    quarters — data it never saw during fitting.
# -----------------------------------------------------------------------------
nValid <- 19
nTrain <- length(revenue.ts) - nValid
train.ts <- window(revenue.ts, end   = time(revenue.ts)[nTrain])
valid.ts <- window(revenue.ts, start = time(revenue.ts)[nTrain + 1])

# -----------------------------------------------------------------------------
# 3. The model ladder (all fit on train.ts)
# -----------------------------------------------------------------------------

## 3a. Regression family ---------------------------------------------------
# Seasonality only  -> captures the Q4 spike but has no growth term (weak).
# + linear trend    -> adds steady growth (strong fit).
# + quadratic trend  -> allows growth to bend.
model_season <- tslm(train.ts ~ season)
model_linear <- tslm(train.ts ~ trend + season)
model_quad   <- tslm(train.ts ~ trend + I(trend^2) + season)

## 3b. ARIMA family --------------------------------------------------------
# Fixed seasonal ARIMA vs. auto-selected order.
arima.fixed <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
arima.auto  <- auto.arima(train.ts)

## 3c. Two-level hybrid (engineered) ---------------------------------------
# A clean-looking regression can still leave structure in its residuals.
# The residual ACF spikes at lag 1 and lag 4 (AR(1) coef ~0.95), so model
# the residuals with AR(1) and add that correction back on top.
reg.quad <- tslm(train.ts ~ trend + I(trend^2) + season)
reg.pred <- forecast(reg.quad, h = nValid, level = 0)
ar1.res      <- Arima(reg.quad$residuals, order = c(1,0,0))   # this is the "why"
ar1.res.pred <- forecast(ar1.res, h = nValid, level = 0)
combined.pred <- reg.pred$mean + ar1.res.pred$mean            # two-level forecast

# -----------------------------------------------------------------------------
# 4. Score every model on the held-out window (MAPE = mean abs % error)
# -----------------------------------------------------------------------------
forecasts <- list(
  "Seasonality only"            = forecast(model_season, h = nValid, level = 0)$mean,
  "Quadratic trend + seasonal"  = forecast(model_quad,   h = nValid, level = 0)$mean,
  "Two-level hybrid"            = combined.pred,
  "Linear trend + seasonal"     = forecast(model_linear, h = nValid, level = 0)$mean,
  "Fixed ARIMA"                 = forecast(arima.fixed,  h = nValid, level = 0)$mean,
  "Auto-ARIMA"                  = forecast(arima.auto,   h = nValid, level = 0)$mean
)

results <- data.frame(
  model = names(forecasts),
  MAPE  = sapply(forecasts, function(f) accuracy(f, valid.ts)[, "MAPE"]),
  row.names = NULL
)
results <- results[order(results$MAPE), ]
print(results)

# -----------------------------------------------------------------------------
# 5. The comparison chart (portfolio hero visual)
#    One picture answers "which method, and how much better?"
# -----------------------------------------------------------------------------
results$model <- factor(results$model, levels = rev(results$model))
results$winner <- results$MAPE == min(results$MAPE)

ggplot(results, aes(x = model, y = MAPE, fill = winner)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", MAPE)),
            hjust = -0.15, size = 4, color = "#1e293b") +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "#c7cdd9", "TRUE" = "#4f46e5"),
                    guide = "none") +
  scale_y_continuous(limits = c(0, 31), expand = c(0, 0)) +
  labs(title = "Forecast accuracy on held-out data",
       subtitle = "Validation-window MAPE  ·  lower is better  ·  scored on 19 quarters never seen in training",
       x = NULL, y = "MAPE (%)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

# ggsave("walmart_forecast_benchmark.svg", width = 8, height = 5)

# -----------------------------------------------------------------------------
# 6. Final forecast — refit the chosen approach on ALL data, project 9 quarters
# -----------------------------------------------------------------------------
final.model <- auto.arima(revenue.ts)
final.fc    <- forecast(final.model, h = 9, level = c(80, 95))
autoplot(final.fc) + labs(title = "Walmart revenue — 9-quarter forecast")
