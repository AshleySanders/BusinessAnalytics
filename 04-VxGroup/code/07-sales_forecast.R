## Predictive model

library(here)
library(readr)
library(tidyverse)
library(janitor)
library(lubridate)
library(forcats)
library(ggplot2)
library(ggpubr)
library(caret)
library(pROC)
library(ranger)
library(xgboost)
library(Matrix)
library(SHAPforxgboost)

######################
## Data Preparation ##
######################
vx_fulljoin <- read_csv(here("100-Projects", "04_VxGroup","data_clean", "vx_fulljoin.csv"))

# Clean & transform the data
vx_data <- vx_fulljoin %>%
  filter(
    !is.na(continent),
    !is.na(deal_amount),
    !is.na(days_to_close)
  ) %>%
  mutate(
    deal_amount = as.numeric(deal_amount),
    closed_won_count = as.numeric(closed_won_count),
    days_to_close = as.numeric(days_to_close),
    primary_company_id = primary_company_id_x
  ) %>%
  clean_names()

# confirm uniqueness in engagement_unique before join
engagement_unique %>%
  count(contact_id) %>%
  filter(n > 1)  # Should return 0 rows

# Clean join between deals and engagements
sales <- left_join(vx_data, engagement_unique,
                   by = "contact_id",
                   relationship = "many-to-one",
                   suffix = c("", ".right")) %>%
  select(-ends_with(".right")) %>%
  clean_names()

# Confirm match-rate after join
nrow(vx_clean)
nrow(sales)  # Should match unless there are contacts in deals not in engagement
sum(is.na(sales$engagement_score_raw))  # How many didn’t get matched

# Drop NAs
sales <- sales %>%
  filter(!is.na(engagement_score_raw))

# Check counts of closed-won deals
table(sales$closed_won_count, useNA = "always")

# Select relevant predictors and keys
sales_model <- sales %>%
  select(
    email,
    contact_id,
    deal_id,
    primary_company_id,
    closed_won_count,
    deal_amount,
    forecast_amount,
    create_date,
    close_date,
    days_to_close,
    deal_stage,
    continent,
    primary_industry,
    original_traffic_source,
    number_of_times_contacted,
    number_of_pageviews,
    number_of_sessions,
    number_of_form_submissions,
    marketing_emails_opened,
    marketing_emails_clicked,
    marketing_emails_replied,
    currently_in_sequence,
    recent_sales_email_replied,
    number_of_times_contacted,
    lifecycle_stage,
    last_contacted,
    last_activity_date,
    role,
    engagement_score_raw
  )

# Convert the target variable to factor first
sales_model <- sales_model %>%
  mutate(
    closed_won_count = case_when(
      closed_won_count == 1 ~ "yes",
      closed_won_count == 0 ~ "no",
      TRUE ~ NA_character_
    ),
    closed_won_count = factor(closed_won_count, levels = c("no", "yes"))
  )

# Check again
table(sales_model$closed_won_count, useNA = "always")

# Engineer new features:
# - deal_age in days
# - active deal = deal is not yet closed (either won or lost)
# - is_closing_this_q = will it close this quarter? (If close date is within 3 months from reference)
# - engaged_recently = within 14 days of export

reference_date <- as.POSIXct("2025-04-05 10:23", format = "%Y-%m-%d %H:%M")

sales_model <- sales_model %>%
  mutate(
    create_date_parsed = mdy_hm(create_date),
    close_date_parsed = mdy_hm(close_date),
    last_contacted_parsed = mdy_hm(last_contacted),

    deal_age = as.numeric(difftime(reference_date, create_date_parsed, units = "days")),
    active_deal = is.na(close_date_parsed),

    is_closing_this_q = if_else(
      !is.na(close_date_parsed) &
        close_date_parsed >= reference_date &
        close_date_parsed <= (reference_date + days(90)), TRUE, FALSE
    ),

    engaged_recently = if_else(
      !is.na(last_contacted_parsed) &
        last_contacted_parsed >= (reference_date - days(14)), TRUE, FALSE
    )
  )

# Cleanup
sales_model <- sales_model %>%
  select(-last_contacted_parsed)


# Impute numeric variables with 0
numeric_vars <- c(
  "deal_amount",
  "forecast_amount",
  "days_to_close",
  "number_of_times_contacted",
  "number_of_pageviews",
  "number_of_sessions",
  "number_of_form_submissions",
  "marketing_emails_opened",
  "marketing_emails_clicked",
  "marketing_emails_replied",
  "engagement_score_raw"
)


sales_model <- sales_model %>%
  mutate(across(all_of(numeric_vars), ~ replace_na(., 0)))

# Impute binary-like logicals
sales_model <- sales_model %>%
  mutate(
    currently_in_sequence = replace_na(currently_in_sequence, FALSE),
    recent_sales_email_replied = replace_na(recent_sales_email_replied, FALSE)
  )

# Impute categorical vars with "unknown"
sales_model <- sales_model %>%
  mutate(
    role = fct_na_value_to_level(as.factor(role), level = "unknown"),
    lifecycle_stage = fct_na_value_to_level(as.factor(lifecycle_stage), level = "unknown"),
    primary_industry = fct_na_value_to_level(as.factor(primary_industry), level = "unknown"),
    continent = fct_na_value_to_level(as.factor(continent), level = "unknown"),
    deal_stage = fct_na_value_to_level(as.factor(deal_stage), level = "unknown"),
    original_traffic_source = fct_na_value_to_level(as.factor(original_traffic_source), level = "unknown")
  )

sales_model <- sales_model %>%
  filter(!is.na(create_date_parsed))  # already parsed date is safer

# Ensure that any NA values in the target variable are dropped and check the total number
sum(is.na(sales_model$closed_won_count))  # How many rows would be dropped?

sales_model <- sales_model %>%
  filter(!is.na(closed_won_count))

# Ensure all NAs have been imputed
sales_model <- sales_model %>%
  # 1. Impute numerics
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%

  # 2. Impute logicals
  mutate(across(where(is.logical), ~ replace_na(., FALSE))) %>%

  # 3. Impute categoricals with "unknown"
  mutate(across(where(is.character), ~ replace_na(., "unknown"))) %>%
  mutate(across(where(is.factor), ~ fct_na_value_to_level(., level = "unknown")))

# Collapse multi-level factors
sales_model <- sales_model %>%
  mutate(
    primary_industry = fct_lump(primary_industry, prop = 0.05, other_level = "Other"),
    primary_industry = fct_drop(primary_industry)  # Drop unused levels
  )

common_roles <- c("VP","CFO","COO","Director","Managing","Director","Manager","CEO")

sales_model <- sales_model %>%
  mutate(
    role = if_else(role %in% common_roles, role, "Other"),
    role = factor(role)
  )

# Check Factor Levels
sales_model %>%
  select(where(is.factor)) %>%
  summarise(across(everything(), ~ n_distinct(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_levels") %>%
  arrange(desc(n_levels))

# Add lead scoring data as features
contacts_scored_xgb <- contacts_scored_xgb %>%
  mutate(contact_id = as.character(contact_id))

sales_model <- sales_model %>%
  mutate(contact_id = as.character(contact_id)) %>%
  left_join(
    contacts_scored_xgb %>% select(contact_id, lead_score_xgb, predicted_prob_xgb, lead_score_tier_xgb),
    by = "contact_id"
  )

# CLean & encode the new features
sales_model <- sales_model %>%
  mutate(lead_score_tier_xgb = factor(lead_score_tier_xgb, levels = c("Low", "Medium", "High")),
         lead_score_xgb = as.numeric(lead_score_xgb))



# Create subset for forecast - predict deal_amount
forecast_cutoff <- as.POSIXct("2025-07-05", format = "%Y-%m-%d")  # 3 months after reference_date

forecast_data <- sales_model %>%
  filter(
    !is.na(close_date_parsed),
    close_date_parsed >= reference_date,
    close_date_parsed <= forecast_cutoff
  ) %>%
  filter(!is.na(deal_amount), deal_amount > 0)

# Train regression model (XGBoost)
# Prepare data
drop_vars_reg <- c(
  "email", "contact_id", "deal_id", "primary_company_id",
  "create_date", "close_date", "last_contacted", "last_activity_date",
  "closed_won_count", "deal_stage", "is_closing_this_q", "active_deal"
)

regression_data <- forecast_data %>%
  select(-all_of(drop_vars_reg)) %>%
  mutate(lead_score_xgb = replace_na(lead_score_xgb, 0))  # just in case


# Create training matrix
x_matrix <- model.matrix(deal_amount ~ . -1, data = regression_data)
y_vector <- regression_data$deal_amount

# Split
set.seed(42)
train_idx <- createDataPartition(y_vector, p = 0.8, list = FALSE)
x_train <- x_matrix[train_idx, ]
x_test <- x_matrix[-train_idx, ]
y_train <- y_vector[train_idx]
y_test <- y_vector[-train_idx]

# Train model
xgb_reg <- xgboost(
  data = x_train,
  label = y_train,
  objective = "reg:squarederror",
  eval_metric = "rmse",
  nrounds = 100,
  early_stopping_rounds = 10,
  verbose = 1
)

# Evaluate & visualize performance
preds <- predict(xgb_reg, newdata = x_test)

# Error metrics
mae <- mean(abs(preds - y_test))
rmse <- sqrt(mean((preds - y_test)^2))

cat("MAE: ", round(mae, 2), "\n")
cat("RMSE: ", round(rmse, 2), "\n")

# Plot predicted vs actual
ggplot(data.frame(Predicted = preds, Actual = y_test), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  labs(title = "Predicted vs Actual Deal Amount", x = "Actual", y = "Predicted")

# Forecast total revenue
forecast_data$predicted_amount <- predict(xgb_reg, newdata = x_matrix)

monthly_forecast <- forecast_data %>%
  mutate(month = floor_date(close_date_parsed, unit = "month")) %>%
  group_by(month) %>%
  summarise(predicted_revenue = sum(predicted_amount))

ggplot(monthly_forecast, aes(x = month, y = predicted_revenue)) +
  geom_line() +
  labs(title = "Predicted Revenue by Month", x = "Month", y = "Revenue ($)")

##------------------------------------------------------------------------------
# Calculate confidence intervals
# Estimate the standard deviation of the residuals on the test set and use it to define approximate 90% prediction intervals (±1.645 × residual SD).

# Calculate the residuals based on model run above
residuals <- y_test - preds

# Estimate standard deviation of residuals
resid_sd <- sd(residuals, na.rm = TRUE)

# Use z-score for ~90% CI: ±1.645 * sd
z_90 <- 1.645

forecast_data <- forecast_data %>%
  mutate(
    close_month = floor_date(close_date_parsed, "month"),
    lower_ci = predicted_amount - z_90 * resid_sd,
    upper_ci = predicted_amount + z_90 * resid_sd
  )

# Summarize
monthly_forecast_summary <- forecast_data %>%
  group_by(close_month) %>%
  summarise(
    predicted_revenue = sum(predicted_amount, na.rm = TRUE),
    lower_ci = sum(lower_ci, na.rm = TRUE),
    upper_ci = sum(upper_ci, na.rm = TRUE),
    high_tier_leads = sum(lead_score_tier_xgb == "High", na.rm = TRUE),
    total_deals = n()
  ) %>%
  mutate(
    pct_high_tier = round(100 * high_tier_leads / total_deals, 1)
  ) %>%
  select(
    Month = close_month,
    Predicted_Revenue = predicted_revenue,
    Lower_CI = lower_ci,
    Upper_CI = upper_ci,
    High_Tier_Lead_Count = high_tier_leads,
    Total_Deals = total_deals,
    High_Tier_Lead_Share = pct_high_tier
  ) %>%
  arrange(Month)

View(monthly_forecast_summary)

write_csv(monthly_forecast_summary, here("100-Projects", "04-VxGroup", "output", "monthly_forecast_summary.csv"))

##------------------------------------------------------------------------------

# Resulting forecast for May is extremely high => compare it with historic data

historic_monthly_revenue <- sales_model %>%
  filter(!is.na(deal_amount), deal_amount > 0, !is.na(close_date_parsed)) %>%
  mutate(close_month = floor_date(close_date_parsed, "month")) %>%
  group_by(close_month) %>%
  summarise(actual_revenue = sum(deal_amount, na.rm = TRUE)) %>%
  filter(close_month >= as.Date("2022-01-01"))  # or 5 years, if you'd prefer

# Compare with forecast
monthly_revenue_compare <- left_join(historic_monthly_revenue, monthly_forecast_summary, by = c("close_month" = "Month"))

print(monthly_revenue_compare %>% arrange(close_month), n= 43)
View(monthly_revenue_compare)

# Plot it
ggplot(monthly_revenue_compare, aes(x = close_month)) +
  geom_col(aes(y = actual_revenue), fill = "gray70", alpha = 0.7) +
  geom_point(aes(y = Predicted_Revenue), color = "#00BFC4", size = 3) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), color = "#00BFC4", width = 10) +
  labs(title = "Historical vs Forecasted Revenue by Month",
       x = "Month", y = "Revenue ($)",
       subtitle = "Gray = actual; Blue = forecast with 90% CI") +
  theme_minimal()

##------------------------------------------------------------------------------
# Examine the forecast model for May more closely

may_pred <- forecast_data %>%
  filter(close_month == as.Date("2025-05-01")) %>%
  select(contact_id, deal_id, predicted_amount, deal_amount, lead_score_tier_xgb, everything()) %>%
  arrange(desc(predicted_amount))

write_csv(may_pred, here("100-Projects", "04-VxGroup", "output", "may_preds_features.csv"))

# Sanity check
max(sales_model$deal_amount, na.rm = TRUE)

# May predictions are driven by 62 low-tier (unlikely to convert) deals, totaling $556K, with only 4 high tier leads with a total predicted value of $38K

##------------------------------------------------------------------------------

# Compute SHAP Values to examine top features driving the high predicted values in low-tier leads

# Filter only low-tier leads from May
may_low_tier <- forecast_data %>%
  filter(close_month == as.Date("2025-05-01"), lead_score_tier_xgb == "Low") %>%
  drop_na()  # Drop rows with NA to prevent model.matrix errors

# Get training feature names from model
trained_features <- xgb_tuned$finalModel$feature_names

# Create matrix from May low-tier leads
X_may_full <- model.matrix(model_formula, data = may_low_tier)[, -1]
X_may_df <- as.data.frame(X_may_full)

# Ensure all expected columns are present
missing_cols <- setdiff(trained_features, names(X_may_df))
for (col in missing_cols) {
  X_may_df[[col]] <- 0
}

# Reorder and restrict to match training exactly
X_may_df <- X_may_df[, trained_features]

# Convert to matrix and create DMatrix
X_may_matrix <- as.matrix(X_may_df)
dmat_may <- xgb.DMatrix(data = X_may_matrix)

# Assign feature names explicitly
colnames(dmat_may) <- trained_features

# Compute SHAP values
shap_values <- shap.values(xgb_model = xgb_tuned$finalModel, X_train = dmat_may)
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = X_may_matrix)

# Plot
shap.plot.summary(shap_long)

# Get expected column names from trained model
expected_cols <- xgb_tuned$finalModel$feature_names

# Align matrix: Add missing columns as zeros
X_may_df <- as.data.frame(X_may_full)

missing_cols <- setdiff(expected_cols, names(X_may_df))
for (col in missing_cols) {
  X_may_df[[col]] <- 0
}

# Drop extra columns not in the model
X_may_df <- X_may_df[, expected_cols, drop = FALSE]

# Convert to matrix with correct column names
X_may_aligned <- as.matrix(X_may_df)
colnames(X_may_aligned) <- expected_cols

# Convert to DMatrix
dtest_may <- xgb.DMatrix(data = X_may_aligned)

# Predict using xgb.Booster object
shap_preds <- predict(xgb_tuned$finalModel, newdata = dtest_may)

# Add predictions and export
shap_df <- as.data.frame(shap_values$shap_score) %>%
  mutate(
    contact_id = may_low_tier$contact_id,
    deal_id = may_low_tier$deal_id,
    predicted_amount = shap_preds
  ) %>%
  relocate(contact_id, deal_id, predicted_amount)

write_csv(shap_df, here("100-Projects", "04-VxGroup", "output", "may_low_tier_shap_values.csv"))

# Plot top 10 SHAP contributions for the highest predicted deal
top_deal <- shap_df %>%
  arrange(desc(predicted_amount)) %>%
  dplyr::slice

top_shap <- top_deal %>%
  select(-contact_id, -deal_id, -predicted_amount) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "shap_value") %>%
  arrange(desc(abs(shap_value))) %>%
  dplyr::slice(1:10)

ggplot(top_shap, aes(x = reorder(feature, shap_value), y = shap_value, fill = shap_value > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 10 SHAP Contributions – Highest Predicted Low-Tier Deal",
       x = "Feature", y = "SHAP Value (Impact on Prediction)") +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

## High Tier May SHAP values

# Compute SHAP Values to examine top features driving the high predicted values in low-tier leads

# Filter only high-tier leads from May
may_hi_tier <- forecast_data %>%
  filter(close_month == as.Date("2025-05-01"), lead_score_tier_xgb == "High") %>%
  drop_na()  # Drop rows with NA to prevent model.matrix errors

# Get training feature names from model
trained_features <- xgb_tuned$finalModel$feature_names

# Create matrix from May low-tier leads
X_may_full <- model.matrix(model_formula, data = may_hi_tier)[, -1]
X_may_df <- as.data.frame(X_may_full)

# Ensure all expected columns are present
missing_cols <- setdiff(trained_features, names(X_may_df))
for (col in missing_cols) {
  X_may_df[[col]] <- 0
}

# Reorder and restrict to match training exactly
X_may_df <- X_may_df[, trained_features]

# Convert to matrix and create DMatrix
X_may_matrix <- as.matrix(X_may_df)
dmat_may <- xgb.DMatrix(data = X_may_matrix)

# Assign feature names explicitly
colnames(dmat_may) <- trained_features

# Compute SHAP values
shap_values <- shap.values(xgb_model = xgb_tuned$finalModel, X_train = dmat_may)
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = X_may_matrix)

# Plot
shap.plot.summary(shap_long)

# Get expected column names from trained model
expected_cols <- xgb_tuned$finalModel$feature_names

# Align matrix: Add missing columns as zeros
X_may_df <- as.data.frame(X_may_full)

missing_cols <- setdiff(expected_cols, names(X_may_df))
for (col in missing_cols) {
  X_may_df[[col]] <- 0
}

# Drop extra columns not in the model
X_may_df <- X_may_df[, expected_cols, drop = FALSE]

# Convert to matrix with correct column names
X_may_aligned <- as.matrix(X_may_df)
colnames(X_may_aligned) <- expected_cols

# Convert to DMatrix
dtest_may <- xgb.DMatrix(data = X_may_aligned)

# Predict using xgb.Booster object
shap_preds <- predict(xgb_tuned$finalModel, newdata = dtest_may)

# Add predictions and export
shap_df <- as.data.frame(shap_values$shap_score) %>%
  mutate(
    contact_id = may_hi_tier$contact_id,
    deal_id = may_hi_tier$deal_id,
    predicted_amount = shap_preds
  ) %>%
  relocate(contact_id, deal_id, predicted_amount)

write_csv(shap_df, here("100-Projects", "04-VxGroup", "output", "may_high_tier_shap_values.csv"))

low_shap_df <- read_csv(here("100-Projects", "04-VxGroup", "output", "may_low_tier_shap_values.csv"))

high_shap_df <- read_csv(here("100-Projects", "04-VxGroup", "output", "may_high_tier_shap_values.csv"))

low_shap_long <- low_shap_df %>%
  select(-contact_id, -deal_id, -predicted_amount) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "shap_value") %>%
  group_by(feature) %>%
  summarise(avg_shap = mean(shap_value), .groups = "drop") %>%
  mutate(tier = "Low")

high_shap_long <- high_shap_df %>%
  select(-contact_id, -deal_id, -predicted_amount) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "shap_value") %>%
  group_by(feature) %>%
  summarise(avg_shap = mean(shap_value), .groups = "drop") %>%
  mutate(tier = "High")

# Combine both into one dataframe
shap_comparison <- bind_rows(low_shap_long, high_shap_long)

# Plot average SHAP value comparison
ggplot(shap_comparison, aes(x = reorder(feature, avg_shap), y = avg_shap, fill = tier)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Average SHAP Value by Lead Tier (May Deals)",
    x = "Feature",
    y = "Average SHAP Contribution",
    fill = "Lead Tier"
  ) +
  theme_minimal()

shap_wide <- low_shap_long %>%
  rename(avg_shap_low = avg_shap) %>%
  left_join(high_shap_long %>% rename(avg_shap_high = avg_shap), by = "feature") %>%
  mutate(shap_diff = avg_shap_high - avg_shap_low) %>%
  arrange(desc(abs(shap_diff)))

shap_wide


##------------------------------------------------------------------------------

##################################
## Revised Regression Forecast ##
#################################

# Feature Engineering - Encode - based on lead score

sales_model <- sales_model %>%
  mutate(
    lead_score_tier_num = case_when(
      lead_score_tier_xgb == "Low" ~ 1,
      lead_score_tier_xgb == "Medium" ~ 2,
      lead_score_tier_xgb == "High" ~ 3,
      TRUE ~ NA_real_
    )
  )

regression_data <- sales_model %>%
  select(-all_of(drop_vars_reg)) %>%
  filter(!is.na(lead_score_tier_num), !is.na(deal_amount), deal_amount > 0)

# Separate forecast and training sets
forecast_data <- sales_model %>%
  filter(
    !is.na(close_date_parsed),
    close_date_parsed >= reference_date,
    close_date_parsed <= forecast_cutoff,
    deal_amount > 0
  )

historical_data <- sales_model %>%
  filter(
    !is.na(close_date_parsed),
    close_date_parsed < reference_date,
    deal_amount > 0,
    !is.na(lead_score_tier_num)
  )

drop_vars_reg <- c(
  "email", "contact_id", "deal_id", "primary_company_id",
  "create_date", "close_date", "last_contacted", "last_activity_date",
  "closed_won_count", "deal_stage", "is_closing_this_q", "active_deal"
)

regression_data <- historical_data %>%
  select(-all_of(drop_vars_reg))

# Create model matrix
x_matrix <- model.matrix(deal_amount ~ . -1, data = regression_data)
y_vector <- regression_data$deal_amount
weights <- regression_data$lead_score_tier_num


set.seed(42)
train_idx <- createDataPartition(y_vector, p = 0.8, list = FALSE)

x_train <- x_matrix[train_idx, ]
x_test <- x_matrix[-train_idx, ]
y_train <- y_vector[train_idx]
y_test <- y_vector[-train_idx]
weight_train <- weights[train_idx]
weight_test <- weights[-train_idx]


# Train model
xgb_weighted <- xgboost(
  data = x_train,
  label = y_train,
  weight = weight_train,
  objective = "reg:squarederror",
  eval_metric = "rmse",
  nrounds = 100,
  early_stopping_rounds = 10,
  verbose = 1
)

# Evaluate & visualize performance
preds <- predict(xgb_reg, newdata = x_test)

# Error metrics
mae <- mean(abs(preds - y_test))
rmse <- sqrt(mean((preds - y_test)^2))

cat("MAE: ", round(mae, 2), "\n")
cat("RMSE: ", round(rmse, 2), "\n")

# Plot predicted vs actual
ggplot(data.frame(Predicted = preds, Actual = y_test), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
  labs(title = "Predicted vs Actual Deal Amount", x = "Actual", y = "Predicted")

# Forecast total revenue
forecast_data$predicted_amount <- predict(xgb_reg, newdata = x_matrix)

monthly_forecast <- forecast_data %>%
  mutate(month = floor_date(close_date_parsed, unit = "month")) %>%
  group_by(month) %>%
  summarise(predicted_revenue = sum(predicted_amount))

ggplot(monthly_forecast, aes(x = month, y = predicted_revenue)) +
  geom_line() +
  labs(title = "Predicted Revenue by Month", x = "Month", y = "Revenue ($)")

##------------------------------------------------------------------------------
# Calculate confidence intervals
# Estimate the standard deviation of the residuals on the test set and use it to define approximate 90% prediction intervals (±1.645 × residual SD).

# Calculate the residuals based on model run above
residuals <- y_test - preds

# Estimate standard deviation of residuals
resid_sd <- sd(residuals, na.rm = TRUE)

# Use z-score for ~90% CI: ±1.645 * sd
z_90 <- 1.645

forecast_data <- forecast_data %>%
  mutate(
    close_month = floor_date(close_date_parsed, "month"),
    lower_ci = predicted_amount - z_90 * resid_sd,
    upper_ci = predicted_amount + z_90 * resid_sd
  )

# Summarize
monthly_forecast_summary <- forecast_data %>%
  group_by(close_month) %>%
  summarise(
    predicted_revenue = sum(predicted_amount, na.rm = TRUE),
    lower_ci = sum(lower_ci, na.rm = TRUE),
    upper_ci = sum(upper_ci, na.rm = TRUE),
    high_tier_leads = sum(lead_score_tier_xgb == "3", na.rm = TRUE),
    total_deals = n()
  ) %>%
  mutate(
    pct_high_tier = round(100 * high_tier_leads / total_deals, 1)
  ) %>%
  select(
    month = close_month,
    predicted_revenue = predicted_revenue,
    lower_CI = lower_ci,
    upper_CI = upper_ci,
    high_tier_lead_count = high_tier_leads,
    total_deals = total_deals,
    high_tier_lead_share = pct_high_tier
  ) %>%
  arrange(month)

View(monthly_forecast_summary)
