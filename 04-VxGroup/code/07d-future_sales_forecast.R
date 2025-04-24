

# Based on the code in 07a-sales_forecast.R

# Reset to the original (full) model matrix and vector
x_matrix <- model.matrix(deal_amount ~ . -1, data = regression_data)
y_vector <- regression_data$deal_amount
weights <- regression_data$lead_score_tier_num

# Redo the split
set.seed(42)
train_idx <- createDataPartition(y_vector, p = 0.8, list = FALSE)

x_train <- x_matrix[train_idx, ]
x_test <- x_matrix[-train_idx, ]
y_train <- y_vector[train_idx]
y_test <- y_vector[-train_idx]
weights_train <- weights[train_idx]
weights_test <- weights[-train_idx]

# Predict on test set
preds_test <- predict(xgb_tuned, newdata = x_test)

##------------------------------------------------------------------------------

## PREDICT DEAL AMOUNTS ON FUTURE DEALS

# Ensure forecast_data is using the same columns
forecast_data_matrix <- model.matrix(deal_amount ~ . -1, data = forecast_data)

# Get column names from training
train_cols <- colnames(x_train)

# Match columns between forecast_data and training matrix
# Add missing columns (set to 0), drop extra ones if any
forecast_data_matrix_aligned <- forecast_data_matrix[, train_cols, drop = FALSE]

# Predict using aligned matrix
forecast_preds <- predict(xgb_tuned, newdata = forecast_data_matrix_aligned)

# Attach predictions back to forecast_data
forecast_data <- forecast_data %>%
  mutate(predicted_amount = forecast_preds)

# Extra columns
setdiff(colnames(forecast_data_matrix), train_cols)

# Missing columns
setdiff(train_cols, colnames(forecast_data_matrix))


# Add forecast month
forecast_data_summary <- forecast_data %>%
  mutate(close_month = floor_date(close_date_parsed, unit = "month")) %>%
  group_by(close_month) %>%
  summarise(
    n_deals = n(),
    total_predicted_sales = sum(predicted_amount, na.rm = TRUE),
    avg_predicted_deal = mean(predicted_amount, na.rm = TRUE)
  ) %>%
  arrange(close_month)

forecast_data_summary # View forecast

##------------------------------------------------------------------------------

## Correcting for many low-probability deals that are overinflating sales predictions

# Attach predictions to forecast_data
forecast_data <- forecast_data %>%
  mutate(predicted_amount = forecast_preds)

expected_deal_value = forecast_data$predicted_amount * forecast_data$predicted_prob_xgb

# Re-aggregate to generate a probability-weighted revenue forecast
forecast_data_summary <- forecast_data %>%
  mutate(
    expected_deal_value = predicted_deal_amount * predicted_prob_xgb,
    close_month = floor_date(close_date_parsed, unit = "month")
  ) %>%
  group_by(close_month) %>%
  summarise(
    n_deals = n(),
    total_expected_sales = sum(expected_deal_value, na.rm = TRUE),
    avg_expected_deal = mean(expected_deal_value, na.rm = TRUE)
  ) %>%
  arrange(close_month)

forecast_data_summary



# Calculate expected deal value per deal
forecast_data <- forecast_data %>%
  mutate(expected_deal_value = predicted_deal_amount * predicted_prob_xgb)

# Summarize forecasted revenue by lead score tier and industry
forecast_summary_by_tier_industry <- forecast_data %>%
  mutate(close_month = floor_date(close_date_parsed, unit = "month")) %>%
  group_by(close_month, lead_score_tier_xgb, primary_industry) %>%
  summarise(
    n_deals = n(),
    total_expected_sales = sum(expected_deal_value, na.rm = TRUE),
    avg_expected_deal = mean(expected_deal_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(close_month, desc(total_expected_sales))

# View the results
forecast_summary_by_tier_industry

Optional: Save to CSV for review later

write_csv(
  forecast_summary_by_tier_industry,
  here("100-Projects", "04-VxGroup", "output", "forecast_summary_by_tier_industry.csv")
)
