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
library(xgboost)
library(Matrix)

######################
## Data Preparation ##
######################

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

lead_score_model <- sales_model %>%
  select(
    contact_id,
    email,
    closed_won_count,
    number_of_pageviews,
    number_of_sessions,
    number_of_form_submissions,
    marketing_emails_opened,
    marketing_emails_clicked,
    marketing_emails_replied,
    currently_in_sequence,
    recent_sales_email_replied,
    lifecycle_stage,
    role,
    engagement_score_raw
  )

# Convert the target variable to factor first
lead_score_model <-lead_score_model %>%
  mutate(
    closed_won_count = case_when(
      closed_won_count == 1 ~ "yes",
      closed_won_count == 0 ~ "no",
      TRUE ~ NA_character_
    ),
    closed_won_count = factor(closed_won_count, levels = c("no", "yes"))
  )

# Check again
table(lead_score_model$closed_won_count, useNA = "always")


# Impute numeric variables with 0
numeric_vars <- c(
  "number_of_pageviews",
  "number_of_sessions",
  "number_of_form_submissions",
  "marketing_emails_opened",
  "marketing_emails_clicked",
  "marketing_emails_replied",
  "engagement_score_raw"
)

lead_score_model <- lead_score_model %>%
  mutate(across(all_of(numeric_vars), ~ replace_na(., 0)))

# Impute binary-like logicals
lead_score_model <- lead_score_model %>%
  mutate(
    currently_in_sequence = replace_na(currently_in_sequence, FALSE),
    recent_sales_email_replied = replace_na(recent_sales_email_replied, FALSE)
  )

# Impute categorical vars with "unknown"
lead_score_model <- lead_score_model %>%
  mutate(
    role = fct_na_value_to_level(as.factor(role), level = "unknown"),
    lifecycle_stage = fct_na_value_to_level(as.factor(lifecycle_stage), level = "unknown")
  )


# Set up caret training control
ctrl <- trainControl(
  method = "cv",
  number = 5,
  sampling = "up", # Upsampling
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Split the data
set.seed(123)
train_index <- createDataPartition(lead_score_model$closed_won_count, p = 0.8, list = FALSE)
train_data <- lead_score_model[train_index, ]
test_data <- lead_score_model[-train_index, ]

# Drop identification fields
train_data_model <- train_data %>% select(-c(contact_id, email))
test_data_model <- test_data %>% select(-c(contact_id, email))

# Explicitly create the model
model_formula <- closed_won_count ~
  number_of_pageviews +
  number_of_sessions +
  number_of_form_submissions +
  marketing_emails_opened +
  marketing_emails_clicked +
  marketing_emails_replied +
  currently_in_sequence +
  recent_sales_email_replied +
  lifecycle_stage +
  role +
  engagement_score_raw

x_train <- model.matrix(model_formula, data = train_data_model)[, -1]  # remove intercept
x_test <- model.matrix(model_formula, data = test_data_model)[, -1]

y_test <- factor(test_data_model$closed_won_count, levels = c("no", "yes"))
y_train <- factor(train_data_model$closed_won_count, levels = c("no", "yes"))

# Double check
nrow(x_test) == length(y_test)  # Should be TRUE

# Tuning hyperparameters
xgb_grid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),             # Learning rate
  gamma = c(0, 1),                     # Minimum loss reduction
  colsample_bytree = c(0.6, 0.8, 1.0), # Subsample columns per tree
  min_child_weight = c(1, 5),          # Min obs in terminal node
  subsample = c(0.7, 1.0)              # Row subsampling
)

# Train with caret::train() Using This Grid
set.seed(123)

xgb_tuned <- train(
  x = x_train,
  y = y_train,
  method = "xgbTree",
  metric = "ROC",  # AUC
  trControl = ctrl,
  tuneGrid = xgb_grid
)

# Best parameters and model performance
xgb_tuned$bestTune
print(xgb_tuned)

# Variable importance
plot(varImp(xgb_tuned), top = 10, main = "XGBoost - Top Predictors")
print(varImp(xgb_tuned), top = 10)

# Make Predictions and Evaluate on Test Set
xgb_preds_tuned <- predict(xgb_tuned, newdata = x_test, type = "prob")

# Evaluate ROC AUC
xgb_roc_tuned <- roc(response = y_test, predictor = xgb_preds_tuned$yes)
auc(xgb_roc_tuned)
plot(xgb_roc_tuned, main = "ROC Curve - XGBoost (Tuned)")

# Predict probabilities using tuned xgboost model
xgb_preds_tuned <- predict(xgb_tuned, newdata = x_test, type = "prob")

# Add predictions to test_data_clean
test_scored_xgb <- test_data_model %>%
  mutate(predicted_prob_xgb = xgb_preds_tuned$yes)

# Join back the original email and contact_id for reference
test_scored_xgb <- test_data %>%
  select(contact_id, email) %>%
  bind_cols(test_scored_xgb)

nrow(test_data) == nrow(test_scored_xgb)  # Check. Should be TRUE


# Scale to 0–100 lead score
test_scored_xgb <- test_scored_xgb %>%
  mutate(
    lead_score_xgb = round(predicted_prob_xgb * 100, 0),
    lead_score_lower_xgb = pmax(0, lead_score_xgb - 10),  # Confidence band lower
    lead_score_upper_xgb = pmin(100, lead_score_xgb + 10) # Confidence band upper
  )

# Determine tier breakpoints
summary(test_scored_xgb$predicted_prob_xgb)

# Define tiers
test_scored_xgb <- test_scored_xgb %>%
  mutate(
    quantile_rank = ntile(predicted_prob_xgb, 100),  # percentiles
    lead_score_tier_xgb = case_when(
      quantile_rank > 70 ~ "High",
      quantile_rank > 40 ~ "Medium",
      TRUE ~ "Low"
    )
  )

# Check distribution of tiers
table(test_scored_xgb$lead_score_tier_xgb)
prop.table(table(test_scored_xgb$lead_score_tier_xgb))


# Visualize the results:
ggplot(test_scored_xgb, aes(x = lead_score_xgb, fill = lead_score_tier_xgb)) +
  geom_histogram(binwidth = 5, color = "white", alpha = 0.85) +
  scale_fill_manual(values = c("Low" = "#F8766D", "Medium" = "#FDBF6F", "High" = "#00BFC4")) +
  labs(
    title = "Lead Scores by Tier (XGBoost Model)",
    x = "Lead Score (0–100)",
    y = "Number of Contacts",
    fill = "Tier"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )


# Conversion rate by XGBoost lead score tier
xgb_conversion <- test_scored_xgb %>%
  group_by(lead_score_tier_xgb) %>%
  summarise(
    total_contacts = n(),
    closed_deals = sum(closed_won_count == "yes"),
    conversion_rate = closed_deals / total_contacts
  )

print(xgb_conversion)


write_csv(test_scored_xgb, here("100-Projects", "04-VxGroup", "data_clean", "xgb_lead_scores.csv"))

