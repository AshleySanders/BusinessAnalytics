library(tidyverse)
library(lubridate)
library(janitor)
library(forcats)
library(here)
library(caret)
library(lubridate)
library(xgboost)
library(SHAPforxgboost)

######################
## Data Preparation ##
######################

## CLEANING & TRANSFORMATION

vx_fulljoin <- read_csv(here("100-Projects", "04_VxGroup","data_clean", "vx_fulljoin.csv"))

contacts_scored_xgb <- read_csv(here("100-Projects", "04-VxGroup", "output", "all_contacts_close_month.csv"))


vx_data <- vx_fulljoin %>%
  filter(!is.na(continent), !is.na(deal_amount), !is.na(days_to_close)) %>%
  mutate(
    deal_amount = as.numeric(deal_amount),
    closed_won_count = as.numeric(closed_won_count),
    days_to_close = as.numeric(days_to_close),
    primary_company_id = primary_company_id_x
  ) %>%
  clean_names()

# Clean join: vx_data + engagement_unique
engagement_unique <- engagement_unique %>%
  mutate(contact_id = as.character(contact_id))

sales <- vx_data %>%
  mutate(contact_id = as.character(contact_id)) %>%
  left_join(engagement_unique, by = "contact_id", relationship = "many-to-one", suffix = c("", ".right")) %>%
  select(-ends_with(".right")) %>%
  clean_names()

# Join in lead scores
contacts_scored_xgb_unique <- contacts_scored_xgb %>%
  mutate(contact_id = as.character(contact_id)) %>%
  distinct(contact_id, .keep_all = TRUE)

sales <- sales %>%
  left_join(
    contacts_scored_xgb_unique %>% select(contact_id, lead_score_xgb, predicted_prob_xgb, lead_score_tier_xgb),
    by = "contact_id",
    suffix = c("", ".right")) %>%
  select(-ends_with(".right"))


# Only exclude if engagement score is completely missing
sales_model <- sales %>%
  filter(!is.na(engagement_score_raw))

# Engineer features
reference_date <- as.POSIXct("2025-04-05 10:23", format = "%Y-%m-%d %H:%M")

sales_model <- sales_model %>%
  mutate(
    create_date_parsed = parse_date_time(create_date, orders = c("mdy HMS", "mdy HM", "ymd", "mdy")),
    close_date_parsed = parse_date_time(close_date, orders = c("mdy HMS", "mdy HM", "ymd", "mdy")),
    last_contacted_parsed = parse_date_time(last_contacted, orders = c("mdy HMS", "mdy HM", "ymd", "mdy")),

    deal_age = as.numeric(difftime(reference_date, create_date_parsed, units = "days")),
    active_deal = is.na(close_date_parsed),
    is_closing_this_q = if_else(!is.na(close_date_parsed) & close_date_parsed >= reference_date & close_date_parsed <= reference_date + days(90), TRUE, FALSE),
    engaged_recently = if_else(!is.na(last_contacted_parsed) & last_contacted_parsed >= reference_date - days(14), TRUE, FALSE)
  ) %>%
  select(-last_contacted_parsed)

# Target variable
sales_model <- sales_model %>%
  mutate(
    closed_won_count = case_when(closed_won_count == 1 ~ "yes", closed_won_count == 0 ~ "no", TRUE ~ NA_character_),
    closed_won_count = factor(closed_won_count, levels = c("no", "yes"))
  )

# Imputation
numeric_vars <- c("deal_amount", "forecast_amount", "days_to_close", "number_of_times_contacted", "number_of_pageviews", "number_of_sessions", "number_of_form_submissions", "marketing_emails_opened", "marketing_emails_clicked", "marketing_emails_replied", "engagement_score_raw")

sales_model <- sales_model %>%
  mutate(across(all_of(numeric_vars), ~ replace_na(., 0))) %>%
  mutate(
    currently_in_sequence = replace_na(currently_in_sequence, FALSE),
    recent_sales_email_replied = replace_na(recent_sales_email_replied, FALSE),
    lead_score_xgb = replace_na(as.numeric(lead_score_xgb), 0),
    lead_score_tier_xgb = fct_na_value_to_level(as.factor(lead_score_tier_xgb), level = "Low")
  )

# Categorical imputation
sales_model <- sales_model %>%
  mutate(
    role = fct_na_value_to_level(as.factor(role), level = "unknown"),
    lifecycle_stage = fct_na_value_to_level(as.factor(lifecycle_stage), level = "unknown"),
    primary_industry = fct_na_value_to_level(as.factor(primary_industry), level = "unknown"),
    continent = fct_na_value_to_level(as.factor(continent), level = "unknown"),
    deal_stage = fct_na_value_to_level(as.factor(deal_stage), level = "unknown"),
    original_traffic_source = fct_na_value_to_level(as.factor(original_traffic_source), level = "unknown")
  )

# Clean factor levels
sales_model <- sales_model %>%
  mutate(
    primary_industry = fct_lump(primary_industry, prop = 0.05, other_level = "Other"),
    primary_industry = fct_drop(primary_industry)  # Drop unused levels
  )

##------------------------------------------------------------------------------

## FEATURE PREP & SELECTION

# Set reference and cutoff dates
reference_date <- as.POSIXct("2025-04-05 10:23", format = "%Y-%m-%d %H:%M")
forecast_cutoff <- reference_date + months(3)

# Add numeric version of lead tier for weighting
sales_model <- sales_model %>%
  mutate(
    lead_score_tier_num = case_when(
      lead_score_tier_xgb == "Low" ~ 1,
      lead_score_tier_xgb == "Medium" ~ 2,
      lead_score_tier_xgb == "High" ~ 3,
      TRUE ~ NA_real_
    )
  )

# Select final features needed for modeling
sales_model <- sales_model %>%
  select(
    # ID vars (retained only if needed later)
    contact_id, deal_id,

    # Target and key engineered variables
    deal_amount, close_date_parsed, create_date_parsed, deal_age,

    # Core features (customize as needed)
    days_to_close,
    continent, primary_industry, original_traffic_source,
    number_of_times_contacted, number_of_pageviews, number_of_sessions, number_of_form_submissions,
    marketing_emails_opened, marketing_emails_clicked, marketing_emails_replied,
    currently_in_sequence, recent_sales_email_replied, lifecycle_stage,
    role, engagement_score_raw,

    # Lead score features
    lead_score_xgb, lead_score_tier_xgb, predicted_prob_xgb, lead_score_tier_num,

    # Temporal indicators
    engaged_recently, is_closing_this_q
  )

##------------------------------------------------------------------------------

## TRAINING, TESTING & FUTURE DEALS SUBSETS

# Separate forecast data (holdout) and historical data (for modeling)
forecast_data <- sales_model %>%
  filter(
    !is.na(close_date_parsed),
    close_date_parsed >= reference_date,
    close_date_parsed <= forecast_cutoff,
    deal_amount > 0
  )

# Impute missing close_date_parsed values as reference_date & deal_amount as the median
median_amount <- median(sales_model$deal_amount[sales_model$deal_amount > 0], na.rm = TRUE)

sales_model <- sales_model %>%
  mutate(
    close_date_parsed = if_else(is.na(close_date_parsed), reference_date, close_date_parsed)) %>%
  mutate(deal_amount = if_else(is.na(deal_amount) | deal_amount <= 0, median_amount, deal_amount))


# Now construct historical data
historical_data <- sales_model %>%
  filter(
    close_date_parsed >= as.POSIXct("2022-04-05"),
    close_date_parsed < reference_date
  )

# Drop ID and date fields, as well as role
drop_vars_reg <- c(
  "contact_id", "deal_id",
  "create_date_parsed", "deal_age",
  "role"
)

regression_data <- historical_data %>%
  select(-all_of(drop_vars_reg))

forecast_data <- forecast_data %>%
  select(-all_of(drop_vars_reg))

# Isolate any problematic column
setdiff(colnames(regression_data), "deal_amount") %>%
  walk(function(colname) {
    formula <- as.formula(paste("deal_amount ~", colname))
    tryCatch({
      model.matrix(formula, data = regression_data)
    }, error = function(e) {
      message("Problem with column: ", colname)
      message("Error: ", e$message)
    })
  })

# Create model matrix
x_matrix <- model.matrix(deal_amount ~ . -1, data = regression_data)
y_vector <- regression_data$deal_amount
weights <- regression_data$lead_score_tier_num

# Split
set.seed(42)
train_idx <- createDataPartition(y_vector, p = 0.8, list = FALSE)

x_train <- x_matrix[train_idx, ]
x_test <- x_matrix[-train_idx, ]
y_train <- y_vector[train_idx]
y_test <- y_vector[-train_idx]
weights_train <- weights[train_idx]
weights_test <- weights[-train_idx]

dtrain <- xgb.DMatrix(data = x_train, label = y_train, weight = weights_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

watchlist <- list(train = dtrain, eval = dtest)

# Quick sanity check
cat("Train set size:", length(y_train), "\n")
cat("Test set size:", length(y_test), "\n")

##------------------------------------------------------------------------------

## TUNING & CROSS-VALIDATION

# Define tuning grid
xgb_grid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 5, 7),
  eta = c(0.01, 0.05, 0.1),
  gamma = c(0, 1),
  colsample_bytree = c(0.6, 0.8),
  min_child_weight = c(1, 3),
  subsample = 1  # keeping full sample usage per tree
)

# Set up cross-validation controls
xgb_ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  allowParallel = TRUE
)


##------------------------------------------------------------------------------

## TRAIN MODEL

# Train model with tuning
xgb_tuned <- train(
  x = x_train,
  y = y_train,
  method = "xgbTree",
  trControl = xgb_ctrl,
  tuneGrid = xgb_grid,
  metric = "RMSE",
  weights = weights_train
)

# View best model parameters
xgb_tuned$bestTune

# Predict and evaluate on test set
preds <- predict(xgb_tuned, newdata = x_test)

# Evaluate
mae <- mean(abs(preds - y_test))
rmse <- sqrt(mean((preds - y_test)^2))
r_squared <- cor(y_test, preds)^2

cat("MAE: ", round(mae, 2), "\n")
cat("RMSE: ", round(rmse, 2), "\n")
cat("R-squared: ", round(r_squared, 3), "\n")

##------------------------------------------------------------------------------

## SHAP Analysis & Feature Selection Refinement

# Extract the best xgboost model from caret
xgb_booster <- xgboost::xgb.Booster.complete(xgb_tuned$finalModel)

# Compute SHAP values using the plain matrix you trained on
shap_result <- shap.values(xgb_model = xgb_booster, X_train = x_train)

# Extract top 20 most important features by mean absolute SHAP value
top_features <- shap_result$mean_shap_score %>%
  enframe(name = "Feature", value = "mean_abs_shap") %>%
  arrange(desc(mean_abs_shap)) %>%
  slice_head(n = 20)

# View as a table
print(top_features)

# Optionally, export to CSV for client-friendly sharing
write_csv(top_features, here("100-Projects", "04-VxGroup", "output", "shap_top_features.csv"))

##------------------------------------------------------------------------------

## REDUCED FEATURE MODEL
top_features <- c(
  "days_to_close",
  "predicted_prob_xgb",
  "continent", # base feature
  "number_of_times_contacted",
  "primary_industry", # base feature
  "engagement_score_raw",
  "lead_score_xgb"
)

regression_data_reduced <- regression_data %>%
  select(deal_amount, all_of(top_features))

# Model matrix
x_matrix_reduced <- model.matrix(deal_amount ~ . -1, data = regression_data_reduced)
y_vector_reduced <- regression_data_reduced$deal_amount
weights_reduced <- historical_data$lead_score_tier_num  # match to reduced y_vector

# Split
set.seed(42)
train_idx <- createDataPartition(y_vector_reduced, p = 0.8, list = FALSE)

x_train <- x_matrix_reduced[train_idx, ]
x_test <- x_matrix_reduced[-train_idx, ]
y_train <- y_vector_reduced[train_idx]
y_test <- y_vector_reduced[-train_idx]
weights_train <- weights_reduced[train_idx]
weights_test <- weights_reduced[-train_idx]

# DMatrix
dtrain <- xgb.DMatrix(data = x_train, label = y_train, weight = weights_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

best_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 5,
  eta = 0.05,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 3,
  subsample = 1
)

xgb_model_reduced <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, eval = dtest),
  early_stopping_rounds = 10,
  verbose = 1
)

preds <- predict(xgb_model_reduced, newdata = x_test)

mae <- mean(abs(preds - y_test))
rmse <- sqrt(mean((preds - y_test)^2))
r_squared <- cor(y_test, preds)^2

cat("MAE: ", round(mae, 2), "\n")
cat("RMSE: ", round(rmse, 2), "\n")
cat("R-squared: ", round(r_squared, 3), "\n")

# Worse performance, so I'll stick with the previous model

##------------------------------------------------------------------------------

# Save the model for forecasting
saveRDS(xgb_tuned, "xgb_model_final.rds")

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

# Compute residuals
residuals <- y_test - preds_test

# Plot residuals
plot(preds_test, residuals,
     main = "Residuals vs. Predicted (Test Set)",
     xlab = "Predicted Deal Amount",
     ylab = "Residuals")``
abline(h = 0, col = "red")

# Observations:
#     - Residuals cluster tightly near zero for small and mid-range deals (left side of the x-axis).
#     - Larger deals (right side) have more underprediction — the residuals drop below zero.
#     - There’s a bit of a “funnel” shape, meaning variance in error increases as the predicted amount increases.

# Interpretation:
#     - model predicts small and average deals decently.
#     - for larger deal amounts, it underestimates the actual value — consistently
#     - That suggests nonlinearity or heteroscedasticity

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
