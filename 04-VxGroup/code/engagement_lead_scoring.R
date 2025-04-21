library(here)
library(readr)
library(tidyverse)
library(lubridate)
library(janitor)
library(recipes)
library(caret)
library(pROC)
library(ranger)
library(ggplot2)
library(ggpubr)



# Cleaned deals data set
vx_data <- read_csv(here("100-Projects", "04-VxGroup", "data_clean", "vx_fulljoin.csv"))

glimpse(vx_data)

# Full contacts data set
contacts <- read_csv(here("100-Projects", "04-VxGroup", "data_clean", "all_contacts_all_columns.csv"))

# Select key engagement variables
engagement <- contacts %>%
  select(contact_id,
         email,
         job_title = reconciled_job_title,
         role = role,
         seniority = employment_seniority,
         number_of_pageviews = number_of_pageviews,
         number_of_sessions = number_of_sessions,
         number_of_form_submissions = number_of_form_submissions,
         marketing_emails_opened,
         marketing_emails_clicked,
         marketing_emails_replied,
         recent_sales_email_replied_date,
         currently_in_sequence,
         lifecycle_stage
) %>%
  # Convert logical or character booleans to numeric; replace NAs with reasonable defaults
  mutate(
    currently_in_sequence = ifelse(currently_in_sequence == TRUE | currently_in_sequence == "true", 1, 0),
   recent_sales_email_replied = ifelse(!is.na(recent_sales_email_replied_date), 1, 0),
    number_of_pageviews = replace_na(number_of_pageviews, 0),
    number_of_sessions = replace_na(number_of_sessions, 0),
    number_of_form_submissions = replace_na(number_of_form_submissions, 0),
    marketing_emails_opened = replace_na(marketing_emails_opened, 0),
    marketing_emails_clicked = replace_na(marketing_emails_clicked, 0),
    marketing_emails_replied = replace_na(marketing_emails_replied, 0),
    currently_in_sequence = replace_na(currently_in_sequence, FALSE),,
    recent_sales_email_replied = !is.na(recent_sales_email_replied_date)
    ) %>%
      # Replace NAs with 0 for numeric engagement metrics
mutate(across(c(number_of_pageviews, number_of_sessions, number_of_form_submissions, marketing_emails_opened, marketing_emails_clicked, marketing_emails_replied),
              ~replace_na(as.numeric(.), 0)))


# Engagement score formula
engagement <- engagement %>%
  mutate(
    engagement_score_raw =
      0.2 * number_of_pageviews +
      0.2 * number_of_sessions +
      0.4 * number_of_form_submissions +
      0.5 * marketing_emails_opened +
      1.0 * marketing_emails_clicked +
      1.5 * marketing_emails_replied +
      2 * recent_sales_email_replied +
      1 * currently_in_sequence
  )

# Convert raw scores into 5 tiers (quantile-based)
quantiles <- quantile(engagement$engagement_score_raw, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Check the distribution to determine breakpoints for engagement tiers
summary(engagement$engagement_score_raw)
hist(engagement$engagement_score_raw, breaks = 30, main = "Engagement Score Distribution")

# Create engagement tiers based on distribution
engagement <- engagement %>%
  mutate(
    engagement_tier = case_when(
      engagement_score_raw >= 15 ~ 5,
      engagement_score_raw >= 5 ~ 4,
      engagement_score_raw >= 1 ~ 3,
      engagement_score_raw > 0 ~ 2,
      TRUE ~ 1 # Zero engagement
    )
  )

# View the threshold counts
engagement %>%
  count(engagement_tier)

# Save CSV
write_csv(engagement, here("100-Projects", "04-VxGroup", "data_clean", "engagement_all_contacts.csv"))

# Verify the breakpoints
# Example assuming a deals dataframe exists
merged <- left_join(engagement, vx_data, by = "contact_id")

# Count how many contacts in each tier have deals
merged %>%
  mutate(has_deal = !is.na(deal_id)) %>%
  count(engagement_tier, has_deal) %>%
  pivot_wider(names_from = has_deal, values_from = n, values_fill = 0) %>%
  rename(no_deal = `FALSE`, has_deal = `TRUE`)

# Count how many closed deals in each tier
merged %>%
  filter(closed_won_count == "1") %>%
  count(engagement_tier)

# Conversion rate by tier
merged %>%
  group_by(engagement_tier) %>%
  summarise(
    total_contacts = n(),
    total_deals = sum(!is.na(deal_id)),
    closed_deals = sum(closed_won_count, na.rm = TRUE),
    conversion_rate = closed_deals / total_contacts
  )

# Engagement rates, as currently calculated do not appear to be highly correlated with conversion, so I'm going to move on logistic regression to determine which variables are most strongly related to conversion, and then we can see if they are also indicators of engagement.

merged <- merged %>% clean_names()

# Check outcome variable distribution
table(merged$closed_won_count)

engagement_deals <- merged

# coerce closed_won_count to numeric
merged <- merged %>%
  mutate(closed_won_count = as.numeric(closed_won_count),
         role = role_x)


# Select relevant predictors
model_data <- merged %>%
  select(
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
    seniority,
    engagement_score_raw
  )

# Convert the target variable to factor first
model_data <- model_data %>%
  mutate(
    closed_won_count = case_when(
      closed_won_count == 1 ~ "yes",
      closed_won_count == 0 ~ "no",
      TRUE ~ NA_character_
    ),
    closed_won_count = factor(closed_won_count, levels = c("no", "yes"))
  )

# Check again
table(model_data$closed_won_count, useNA = "always")

# Impute numeric variables with 0
num_vars <- c(
  "number_of_pageviews",
  "number_of_sessions",
  "number_of_form_submissions",
  "marketing_emails_opened",
  "marketing_emails_clicked",
  "marketing_emails_replied",
  "engagement_score_raw"
)


model_data <- model_data %>%
  mutate(across(all_of(num_vars), ~ replace_na(., 0)))

# Impute binary-like logicals
model_data <- model_data %>%
  mutate(
    recent_sales_email_replied = replace_na(recent_sales_email_replied, FALSE),
    currently_in_sequence = if_else(currently_in_sequence == TRUE | currently_in_sequence == "true", TRUE, FALSE)
  )

# Impute categorical vars with "unknown"
model_data <- model_data %>%
  mutate(
    lifecycle_stage = fct_na_value_to_level(as.factor(lifecycle_stage), level = "unknown"),
    role = fct_na_value_to_level(as.factor(role), level = "unknown"),
    seniority = fct_na_value_to_level(as.factor(seniority), level = "unknown")
  )

# Check distribution of target variable before proceeding
table(model_data$closed_won_count, useNA = "always")

model_data <- model_data %>%
  filter(!is.na(closed_won_count))

# Split the data
set.seed(123)
train_index <- createDataPartition(model_data$closed_won_count, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Drop unused factor levels to prevent contrast errors
train_data <- droplevels(train_data)


# Match factor levels in test data to those in training data
for (var in names(train_data)) {
  if (is.factor(train_data[[var]])) {
    test_data[[var]] <- factor(test_data[[var]], levels = levels(train_data[[var]]))
  }
}


# Identify and drop any factor with <= 1 level
factor_vars <- sapply(train_data, is.factor)
drop_vars <- names(train_data)[factor_vars][sapply(train_data[factor_vars], function(x) length(levels(x)) <= 1)]
train_data <- train_data %>% select(-all_of(drop_vars))
test_data <- test_data %>% select(-all_of(drop_vars))
logit_model <- train(
closed_won_count ~ .,
data = train_data,
method = "glm",
family = "binomial",
metric = "ROC",
trControl = ctrl
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

# Keep ID columns in a separate lookup
test_ids <- test_data %>% select(contact_id, email)

# Save a separate version for modeling
train_data_model <- train_data %>% select(-email)
test_data_model <- test_data %>% select(-email)

# Identify and drop factors with only one level
factor_vars <- sapply(train_data_model, is.factor)
drop_vars <- names(train_data_model)[factor_vars][sapply(train_data_model[factor_vars], function(x) length(levels(x)) <= 1)]

# Drop from both train and test data
train_data_model <- train_data_model %>% select(-all_of(drop_vars))
test_data_clean <- test_data_clean %>% select(-all_of(drop_vars))


# Train logistic regression model
logit_model <- train(
  closed_won_count ~ .,
  data = train_data_model,
  method = "glm",
  family = "binomial",
  metric = "ROC",
  trControl = ctrl
)

# Remove rows with NA in predictors used by the model
predictors_used <- names(train_data_model)[names(train_data_model) != "closed_won_count"]
test_data_clean <- test_data_model %>%
  filter(if_all(all_of(predictors_used), ~ !is.na(.)))

# Match factor levels again
for (var in names(train_data_model)) {
  if (is.factor(train_data_model[[var]])) {
    test_data_clean[[var]] <- factor(test_data_clean[[var]], levels = levels(train_data[[var]]))
  }
}

# Predict using cleaned test set
logit_preds <- predict(logit_model, newdata = test_data_clean, type = "prob")

# Ensure matching lengths again
length(logit_preds$yes)            # Should match next line
length(test_data_clean$closed_won_count)

# ROC AUC
roc_obj <- roc(response = test_data_clean$closed_won_count,
               predictor = logit_preds$yes)
auc(roc_obj)
plot(roc_obj, main = "ROC Curve - Logistic Regression")

# The data is sparse, the target variable is moderately imbalanced (32% yes), and high cardinality categorical variables, particularly "role."

# In order to generate a quick, interpretable model with variable importance rankings, I'm going to use ranger, a tree-based (random forest) model with probability estimates.

# Reuse the same training control, assigned to "ctrl", as above.

# Train ranger model
set.seed(123)
rf_model <- train(
  closed_won_count ~ .,
  data = train_data_model,
  method = "ranger",
  metric = "ROC", # Optimize for AUC
  trControl = ctrl,
  importance = "impurity" # or "permutation" for more robust estimates
)

# View Results
print(rf_model)

# Plot variable importance
dev.off()  # Reset graphics device

varImpPlot <- varImp(rf_model, scale = FALSE)
plot(varImpPlot, top = 15, main = "Top 15 Predictors - Ranger Model")

# Table of importance scores:
print(varImpPlot)

# Predict probabilities for test set
rf_preds <- predict(rf_model, newdata = test_data_clean, type = "prob")

# Evaluate ROC
rf_roc <- roc(response = test_data_clean$closed_won_count, predictor = rf_preds$yes)
auc(rf_roc)
plot(rf_roc, main = "ROC Curve - Random Forest")

# This model performs quite well with an AUC of 0.9, specificity of 97% and sensitivity of ~70%.

rf_preds # predicted probabilities

# Combine the predicted probabilities with the test_data_clean set to assign to meaningful tiers for the business
test_scored <- test_data_clean %>%
  mutate(predicted_prob = rf_preds$yes)

# Explore the probability distribution
summary(test_scored$predicted_prob)
hist(test_scored$predicted_prob, breaks = 30, main = "Predicted Conversion Probabilities", xlab = "Probability")

# Define lead score tiers based on the distribution
test_scored <- test_scored %>%
  mutate(lead_score_tier = case_when(
    predicted_prob >= 0.7 ~ "High",
    predicted_prob >= 0.4 ~ "Medium",
    TRUE ~ "Low"
  ))

# Diagnostic Checks
table(test_scored$lead_score_tier)
prop.table(table(test_scored$lead_score_tier))  # % by tier

# Export leads by tier
write_csv(test_scored, here("100-Projects", "04-VxGroup", "data_clean",
          "lead_score_output.csv"))

# Create scaled lead score from predicted probability
test_scored <- test_scored %>%
  mutate(
    lead_score = round(predicted_prob * 100, 0)
  )

# Add confidence band estimates (±10 as a placeholder — customize if needed)
# You can use bootstrapping or percentile-based bands if you have prediction intervals
test_scored <- test_scored %>%
  mutate(
    lead_score_lower = pmax(0, lead_score - 10),  # no less than 0
    lead_score_upper = pmin(100, lead_score + 10) # no more than 100
  )

# Plot predicted probability by lead score tier
dev.off()  # Reset graphics device

ggplot(test_scored, aes(x = lead_score, fill = lead_score_tier)) +
  geom_histogram(binwidth = 5, color = "white", alpha = 0.8) +
  geom_vline(data = data.frame(threshold = c(40, 70)), aes(xintercept = threshold),
             linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("Low" = "#F8766D", "Medium" = "#FDBF6F", "High" = "#00BFC4")) +
  labs(
    title = "Predicted Conversion Scores by Tier",
    x = "Lead Score (0–100)",
    y = "Number of Contacts",
    fill = "Tier"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Export enriched lead score results
write_csv(test_scored %>%
            select(contact_id, email, predicted_prob, lead_score, lead_score_lower, lead_score_upper, lead_score_tier),
          here("100-Projects", "04-VxGroup", "data_clean", "lead_score_output_detailed.csv"))

####################
## xgboost model ##
###################

library(xgboost)
library(Matrix)

# Use test_data_model instead of test_data_clean to ensure consistency
# Drop rows with any NA in predictors
predictors_used <- names(train_data_model)[names(train_data_model) != "closed_won_count"]
test_data_model_clean <- test_data_model %>%
  filter(if_all(all_of(predictors_used), ~ !is.na(.)))

# Create x_test and y_test from the SAME filtered data
x_test <- model.matrix(closed_won_count ~ . - 1, data = test_data_model_clean)
y_test <- test_data_model_clean$closed_won_count
y_test <- factor(y_test, levels = c("no", "yes"))

# Double check
nrow(x_test) == length(y_test)  # Should be TRUE

# Train the model using caret for consistency
xgb_grid <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

xgb_model <- train(
  x = x_train,
  y = y_train,
  method = "xgbTree",
  metric = "ROC",
  trControl = ctrl,
  tuneGrid = xgb_grid
)

# View model results
print(xgb_model)

# Predict probabilities on test set
xgb_preds <- predict(xgb_model, newdata = x_test, type = "prob")
nrow(xgb_preds) == length(y_test)  # Confirm again

# ROC and AUC
xgb_roc <- roc(response = y_test, predictor = xgb_preds$yes)
auc(xgb_roc)
plot(xgb_roc, main = "ROC Curve - XGBoost")

# Attach back email and contact_id
# Bind the predictions with the IDs (same row order assumed)
test_scored_xgb <- bind_cols(test_ids[rownames(x_test), , drop = FALSE],  # Align by row names of x_test
                             predicted_prob_xgb = xgb_preds$yes)


test_scored_xgb <- test_scored_xgb %>%
  mutate(
    lead_score_xgb = round(predicted_prob_xgb * 100),
    lead_score_tier_xgb = case_when(
      predicted_prob_xgb >= 0.7 ~ "High",
      predicted_prob_xgb >= 0.4 ~ "Medium",
      TRUE ~ "Low"
    )
  )

# Optional: save
write_csv(test_scored_xgb, here("100-Projects", "04-VxGroup", "data_clean", "xgb_lead_scores.csv"))


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
plot(varImp(xgb_tuned), top = 15, main = "XGBoost - Top Predictors")

# Make Predictions and Evaluate on Test Set
xgb_preds_tuned <- predict(xgb_tuned, newdata = x_test, type = "prob")

# Evaluate ROC AUC
xgb_roc_tuned <- roc(response = y_test, predictor = xgb_preds_tuned$yes)
auc(xgb_roc_tuned)
plot(xgb_roc_tuned, main = "ROC Curve - XGBoost (Tuned)")

# Compared Tuned vs Untuned
auc(rf_roc)          # Random forest AUC
auc(xgb_roc)         # XGBoost baseline
auc(xgb_roc_tuned)   # Tuned XGBoost

# Predict probabilities using tuned xgboost model
xgb_preds_tuned <- predict(xgb_tuned, newdata = x_test, type = "prob")

# Add predictions to test_data_clean
test_scored_xgb <- test_data_clean %>%
  mutate(predicted_prob_xgb = xgb_preds_tuned$yes)

# Join back the original email and contact_id for reference
test_scored_xgb <- test_scored_xgb %>%
  left_join(test_data %>% select(contact_id, email), by = intersect(names(test_data), names(test_scored_xgb)))

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
      quantile_rank > 72 ~ "High",
      quantile_rank > 40 ~ "Medium",
      TRUE ~ "Low"
    )
  )


# Check distribution of tiers
table(test_scored_xgb$lead_score_tier_xgb)
prop.table(table(test_scored_xgb$lead_score_tier_xgb))

max_y <- test_scored_xgb %>%
  count(lead_score_xgb = cut(lead_score_xgb, breaks = seq(0, 100, 5))) %>%
  pull(n) %>%
  max()

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

# Testing Performance
# Conversion rate by Ranger lead score tier
ranger_conversion <- test_scored %>%
  group_by(lead_score_tier) %>%
  summarise(
    total_contacts = n(),
    closed_deals = sum(closed_won_count == "yes"),
    conversion_rate = closed_deals / total_contacts
  )

# Conversion rate by XGBoost lead score tier
xgb_conversion <- test_scored_xgb %>%
  group_by(lead_score_tier_xgb) %>%
  summarise(
    total_contacts = n(),
    closed_deals = sum(closed_won_count == "yes"),
    conversion_rate = closed_deals / total_contacts
  )

# View both tables
print(ranger_conversion)
print(xgb_conversion)

test_scored_xgb <- bind_cols(test_ids[rownames(x_test), , drop = FALSE],  # Align by row names of x_test
                             predicted_prob_xgb = xgb_preds$yes)

write_csv(test_scored_xgb, here("100-Projects", "04-VxGroup", "data_clean", "xgb_lead_scores.csv"))
