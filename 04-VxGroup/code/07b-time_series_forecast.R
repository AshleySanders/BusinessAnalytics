# Load libraries
library(prophet)
library(dplyr)
library(lubridate)
library(ggplot2)

# Selected prophet since it's a better fit for the trends we see in this data set:
# irregular events, outliers, missing months, and changepoints

monthly_data <- monthly_revenue_compare %>%
  rename(ds = close_month, y = actual_revenue) %>%
  filter(y > 0) %>% # Filter out zero months to improve model stability
  mutate(ds = as.Date(ds)) # Ensure data format

# Fit Prophet model
m <- prophet(monthly_data)

# Make a longer future dataframe (e.g., 12 months)
future <- make_future_dataframe(m, periods = 12, freq = "month")

# Predict
forecast <- predict(m, future)

# Extract forecast for April–June 2025
library(lubridate)
forecast_q2_2025 <- forecast %>%
  filter(month(ds) %in% 4:6, year(ds) == 2025) %>%
  select(ds, yhat, yhat_lower, yhat_upper)

print(forecast_q2_2025)

##------------------------------------------------------------------------------

# Compare time series output with xgboost model output
# Clean up XGBoost summary for merge
xgb_summary_clean <- monthly_forecast_summary %>%
  filter(Month >= as.Date("2025-04-01") & Month <= as.Date("2025-06-30")) %>%
  select(ds = Month, xgb_yhat = Predicted_Revenue,
         xgb_lower = Lower_CI, xgb_upper = Upper_CI)

# Combine with Prophet
comparison_df <- forecast_q2_2025 %>%
  left_join(xgb_summary_clean, by = "ds") %>%
  select(ds,
         prophet_yhat = yhat,
         prophet_lower = yhat_lower,
         prophet_upper = yhat_upper,
         xgb_yhat,
         xgb_lower,
         xgb_upper)

print(comparison_df)


library(ggplot2)

ggplot(comparison_df, aes(x = ds)) +
  geom_line(aes(y = prophet_yhat, color = "Prophet")) +
  geom_ribbon(aes(ymin = prophet_lower, ymax = prophet_upper, fill = "Prophet"), alpha = 0.2) +
  geom_line(aes(y = xgb_yhat, color = "XGBoost")) +
  geom_ribbon(aes(ymin = xgb_lower, ymax = xgb_upper, fill = "XGBoost"), alpha = 0.2) +
  labs(title = "Forecast Comparison: Prophet vs XGBoost",
       x = "Month", y = "Predicted Revenue ($)",
       color = "Model", fill = "Model") +
  scale_color_manual(values = c("Prophet" = "#0072B2", "XGBoost" = "#D55E00")) +
  scale_fill_manual(values = c("Prophet" = "#0072B2", "XGBoost" = "#D55E00")) +
  theme_minimal()
