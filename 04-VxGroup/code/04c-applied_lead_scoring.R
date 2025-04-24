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

# confirm uniqueness in contacts_unique before join
contacts_unique %>%
  count(contact_id) %>%
  filter(n > 1)  # Should return 0 rows


# calculate engagement scores for all contacts
# Select key engagement variables
engagement_all <- contacts_unique %>%
  select(
    contact_id,
    email,
    role,
    number_of_pageviews,
    number_of_sessions,
    number_of_form_submissions,
    marketing_emails_opened,
    marketing_emails_clicked,
    marketing_emails_replied,
    recent_sales_email_replied_date,
    currently_in_sequence,
    lifecycle_stage
    ) %>%
  mutate(
    recent_sales_email_replied = if_else(!is.na(recent_sales_email_replied_date), TRUE, FALSE)
  ) %>%
  select(-recent_sales_email_replied_date)



# Impute numeric variables with 0
numeric_vars <- c(
  "number_of_pageviews",
  "number_of_sessions",
  "number_of_form_submissions",
  "marketing_emails_opened",
  "marketing_emails_clicked",
  "marketing_emails_replied"
)

engagement_all <- engagement_all %>%
  mutate(across(all_of(numeric_vars), ~ replace_na(., 0)))

# Impute binary-like logicals
engagement_all <- engagement_all %>%
  mutate(
    currently_in_sequence = replace_na(currently_in_sequence, FALSE),
    recent_sales_email_replied = replace_na(recent_sales_email_replied, FALSE)
  )

# Impute categorical vars with "unknown"
engagement_all <- engagement_all %>%
  mutate(
    role = fct_na_value_to_level(as.factor(role), level = "unknown"),
    lifecycle_stage = fct_na_value_to_level(as.factor(lifecycle_stage), level = "unknown")
  )

# Engagement score formula
engagement_all <- engagement_all %>%
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
quantiles <- quantile(engagement_all$engagement_score_raw, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Check the distribution to determine breakpoints for engagement tiers
summary(engagement_all$engagement_score_raw)
hist(engagement_all$engagement_score_raw, breaks = 30, main = "Engagement Score Distribution")

# Create engagement tiers based on distribution
engagement_all <- engagement_all %>%
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
engagement_all %>%
  count(engagement_tier)

# Save CSV
write_csv(engagement_all, here("100-Projects", "04-VxGroup", "output", "engagement_all_contacts.csv"))

##-----------------------------------------------------------------------------

## Prepare closed_won_count column for all contact IDs associated with each deal

# Load the deals dataset
deals_all <- read_csv(here("100-Projects", "04-VxGroup", "data_all", "all_deals_all_columns.csv")) %>% clean_names()

# Convert all contact_id columns to character for consistency
deals_all <- deals_all %>%
  mutate(across(starts_with("contact_id"), as.character))

# Select only relevant columns
deals_contacts <- deals_all %>%
  select(closed_won_count = closed_won_count,
         contact_id, contact_id_2, contact_id_3, contact_id_4)

# Pivot to long format
contacts_long <- deals_contacts %>%
  pivot_longer(
    cols = starts_with("contact_id"),
    names_to = "contact_role",
    values_to = "contact_id"
  ) %>%
  filter(!is.na(contact_id)) # drop NA contacts

# Group and sum closed_won_count by contact
closed_won_by_contact <- contacts_long %>%
  group_by(contact_id) %>%
  summarise(closed_won_count = sum(closed_won_count, na.rm = TRUE)) %>%
  ungroup()

# View result
head(closed_won_by_contact)

# Merge into engagement_all
engagement_all <- engagement_all %>%
  mutate(contact_id = as.character(contact_id)) %>%
  left_join(closed_won_by_contact, by = "contact_id") %>%
  mutate(closed_won_count = replace_na(closed_won_count, 0))

##------------------------------------------------------------------------------

all_lead_scores <- engagement_all

# Check counts of closed-won deals
table(all_lead_scores$closed_won_count, useNA = "always")

# Convert the target variable to factor first
all_lead_scores <- all_lead_scores %>%
  mutate(
    closed_won_count = case_when(
      closed_won_count >= 1 ~ "yes",
      closed_won_count == 0 ~ "no",
      TRUE ~ NA_character_
    ),
    closed_won_count = factor(closed_won_count, levels = c("no", "yes"))
  )

# Check again after conversion to factor
table(all_lead_scores$closed_won_count, useNA = "always")

all_lead_scores_model <- all_lead_scores %>%
  select(-c(contact_id, email))

# Impute categorical vars with "unknown"
all_lead_scores_model <- all_lead_scores_model %>%
  mutate(
    role = fct_na_value_to_level(as.factor(role), level = "unknown"),
    lifecycle_stage = fct_na_value_to_level(as.factor(lifecycle_stage), level = "unknown")
  )

##------------------------------------------------------------------------------

########################
## Score all contacts ##
########################

# Explicitly convert variables to match the data types of the model

all_lead_scores_model <- all_lead_scores_model %>%
  mutate(
    marketing_emails_opened = as.numeric(marketing_emails_opened),
    marketing_emails_clicked = as.numeric(marketing_emails_clicked),
    marketing_emails_replied = as.numeric(marketing_emails_replied),
    currently_in_sequence = as.numeric(currently_in_sequence)
  )

# Match factor levels explicitly
for (col in names(train_data_model)) {
  if (is.factor(train_data_model[[col]])) {
    all_lead_scores_model[[col]] <- factor(all_lead_scores_model[[col]], levels = levels(train_data_model[[col]]))
  }
}

new_contacts <- model.matrix(model_formula, data = all_lead_scores_model)[, -1]

# check
stopifnot(nrow(new_contacts) == nrow(all_lead_scores))

# Predict probabilities using tuned xgboost model
xgb_preds_tuned_all <- predict(xgb_tuned, newdata = new_contacts, type = "prob")

# Add predictions to contacts_scored, maintaining contact_id and email from all_lead_scores
contacts_scored_xgb <- all_lead_scores %>%
  mutate(predicted_prob_xgb = xgb_preds_tuned_all$yes)

# Scale to 0–100 lead score
contacts_scored_xgb <- contacts_scored_xgb %>%
  mutate(
    lead_score_xgb = round(predicted_prob_xgb * 100, 0),
    lead_score_lower_xgb = pmax(0, lead_score_xgb - 10),  # Confidence band lower
    lead_score_upper_xgb = pmin(100, lead_score_xgb + 10) # Confidence band upper
  )

# Determine tier breakpoints
summary(test_scored_xgb$predicted_prob_xgb)

# Define tiers

contacts_scored_xgb <- contacts_scored_xgb %>%
  mutate(
    quantile_rank = ntile(predicted_prob_xgb, 100),  # percentiles
    lead_score_tier_xgb = case_when(
      quantile_rank > 70 ~ "High",
      quantile_rank > 40 ~ "Medium",
      TRUE ~ "Low"
    )
  )


# Check distribution of tiers
table(contacts_scored_xgb$lead_score_tier_xgb)
prop.table(table(contacts_scored_xgb$lead_score_tier_xgb))


# Visualize the results:
ggplot(contacts_scored_xgb, aes(x = lead_score_xgb, fill = lead_score_tier_xgb)) +
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

# Export clean scoring file
write_csv(contacts_scored_xgb, here("100-Projects", "04-VxGroup", "output", "all_contacts_scored.csv"))





