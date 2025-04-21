library(here)
library(readr)
library(tidyverse)
library(lubridate)
library(janitor)
library(forcats)
library(survival)
library(survminer)
library(ranger)

# Load data
deals <- read_csv(here("100-Projects", "04-VxGroup", "data_clean", "all_deals_R.csv")) %>% clean_names()

contacts <- read_csv(here("100-Projects", "04-VxGroup", "data_clean", "all_contacts_all_columns.csv")) %>%  clean_names()

# Remove duplicates
contacts_unique <- contacts %>%
  distinct(contact_id, .keep_all = TRUE)

# Join full contact info with deals data
vx_deals <- left_join(deals, contacts_unique, by = "contact_id", suffix = c("", ".right")) %>%
  select(-ends_with(".right"))  # Drop duplicated columns from right table)

# Select and rename relevant columns
vx_deals <- vx_deals %>%
  transmute(
    contact_id,
    primary_company_id,
    name = paste0(first_name, " ", last_name),
    days_to_close = replace_na(as.numeric(days_to_close), 0),
    converted = as.numeric(closed_won_count), # note that the new name

    # Engagement predictors with imputation
    number_of_pageviews = replace_na(as.numeric(number_of_pageviews), 0),
    number_of_sessions = replace_na(as.numeric(number_of_sessions), 0),
    number_of_form_submissions = replace_na(as.numeric(number_of_form_submissions), 0),
    marketing_emails_opened = replace_na(as.numeric(marketing_emails_opened), 0),
    marketing_emails_clicked = replace_na(as.numeric(marketing_emails_clicked), 0),
    marketing_emails_replied = replace_na(as.numeric(marketing_emails_replied), 0),

    # Logical fields to numeric with NA handling
    currently_in_sequence = if_else(
      currently_in_sequence %in% c(TRUE, "true"),
      1,
      0,
      missing = 0
    ),
    recent_sales_email_replied = as.numeric(!is.na(recent_sales_email_replied_date)),

    # Categorical fields
    lifecycle_stage = fct_na_value_to_level(as.factor(lifecycle_stage), level = "unknown"),
    role = fct_na_value_to_level(as.factor(role), level = "unknown"),
    job_domain = fct_na_value_to_level(as.factor(job_domain), level = "unknown"),

    # Sales activity fields
    number_of_times_contacted = replace_na(as.numeric(number_of_times_contacted), 0),
    number_of_sales_activities = replace_na(as.numeric(number_of_sales_activities), 0)
    )

# Ensure that event vars do not have any NA values & that values align with pre-join data
table(vx_deals$converted, useNA = "always")
sum(is.na(vx_deals$days_to_close))

# ✅ Sanity check — ensure all fields are NA-free before calculation
colSums(is.na(vx_deals %>%
                select(
                  number_of_pageviews, number_of_sessions, number_of_form_submissions,
                  marketing_emails_opened, marketing_emails_clicked, marketing_emails_replied,
                  currently_in_sequence, recent_sales_email_replied
                )))

# Recreate the engagement score from the engagement & lead scoring pipeline
# Calculate engagement score
vx_deals <- vx_deals %>%
  mutate(engagement_score =
  0.2 * number_of_pageviews +
  0.2 * number_of_sessions +
  0.4 * number_of_form_submissions +
  0.5 * marketing_emails_opened +
  1.0 * marketing_emails_clicked +
  1.5 * marketing_emails_replied +
  2.0 * recent_sales_email_replied +
  1.0 * currently_in_sequence
)

# Add industry data
companies <- read_csv(here("100-Projects", "04-VxGroup", "data_clean", "companies_unique.csv")) %>% clean_names()

industry_data <- companies %>%
  select(primary_company_id, primary_industry)

vx_deals <- left_join(vx_deals, industry_data, by = "primary_company_id") %>%
  mutate(
    industry = fct_na_value_to_level(as.factor(primary_industry), level = "unknown")
  ) %>%
  select(-primary_industry)  # drop the raw field after processing


# Selected Cox Proportional Hazards Model to try first for the time-to-event (survival) analysis since it takes covariates into account and provides interpretable results.

# Check for multicollinearity
library(car)

# Fit a linear model (for VIF calculation only; doesn't need to be survival)
lm_temp <- lm(engagement_score ~ role + lifecycle_stage + job_domain +
                number_of_times_contacted + number_of_sales_activities + industry, data = vx_deals)

vif(lm_temp) # errors

alias(lm_temp)

# drop job_domain due to multicollinearity with role
lm_temp <- lm(engagement_score ~ role + lifecycle_stage +
                number_of_times_contacted + number_of_sales_activities + industry,
              data = vx_deals)

vif(lm_temp)

 # I dropped job_domain and found that number_of_times_contacted and number_of_sales_activities show moderate multicollinearity. I decided to keep number_of_times_contacted since it's client-facing, more interpretable and actionable and it's less prone to logging bias.

# My decision is also supported by the following Cox models, which shows that number_of_times_contacted has stronger association with conversion timing:
summary(coxph(surv_obj ~ number_of_times_contacted, data = vx_deals))
summary(coxph(surv_obj ~ number_of_sales_activities, data = vx_deals))

# Fit a basic Cox model

# Create the survival object
surv_obj <- Surv(time = vx_deals$days_to_close, event = vx_deals$converted)

# Fit initial Cox model
cox_model <- coxph(surv_obj ~ engagement_score + role + lifecycle_stage + number_of_times_contacted + industry, data = vx_deals)

# View model
summary(cox_model)

# Test proportional hazards assumption
cox_zph <- cox.zph(cox_model) # error: system is computationally singular

# Refit model incrementally to isolate the problem
coxph(surv_obj ~ engagement_score, data = vx_deals)

# suggests collinearity between engagement scores and either conversion or days_to_close, but we don't want to drop engagement scores, since our predictive models (using ranger and xgboost) demonstrated that it's a good predictor of conversion.

# Consequently, we'll use an Accelerated Failure Time (AFT) Model, which is parametric, models time directly, and can often handle correlated predictors better

###############
## AFT Model ##
###############

aft_model <- survreg(
  Surv(days_to_close, converted) ~ engagement_score + role + lifecycle_stage + number_of_times_contacted + industry,
  data = vx_deals,
  dist = "weibull" # we can also try ""lognormal"
) # errors due to invalid survival times (of 0)

# Update the cleaning strategy : replace NAs with max follow-up
max_followup <- max(vx_deals$days_to_close, na.rm = TRUE)

vx_deals <- vx_deals %>%
  mutate(
    days_to_close_clean = case_when(
      is.na(days_to_close) ~ max_followup,
      days_to_close == 0   ~ 1,                # Replace 0 with 1 for converted deals
      TRUE ~ days_to_close
    ),
    converted_event = as.numeric(converted == 1)
  )

vx_deals_clean <- vx_deals %>%
  filter(days_to_close_clean > 0)

# Construct the survival object
surv_obj <- Surv(time = vx_deals_clean$days_to_close_clean, event = vx_deals_clean$converted_event)



# Fit the AFT model again
aft_model <- survreg(
  surv_obj ~ engagement_score + role + lifecycle_stage + number_of_times_contacted + industry,
  data = vx_deals_clean,
  dist = "weibull"
)

# View the AFT model summary
summary(aft_model)

library(broom)

# Create a tidy table of the model results
aft_tidy <- tidy(aft_model)

aft_tidy_with_stars <- aft_tidy %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  )

# View the result
print(aft_tidy_with_stars, n=43)

View(aft_tidy)

# Results were either inconclusive or simply reinforced best practices in sales and marketing.

# I'm going to try one more modeling technique to see if there is something meaningful that could help my client close deals faster.

############################
## Random Survival Forest ##
############################

# Data preparation
# Fill in days_to_close with maximum follow-up time for those who didn't convert
max_followup <- max(vx_deals$days_to_close, na.rm = TRUE)

vx_deals <- vx_deals %>%
  mutate(
    days_to_close_clean = if_else(is.na(days_to_close), max_followup, days_to_close),
    converted_event = as.numeric(converted == 1)
  )


# Build the survival object
surv_obj <- Surv(time = vx_deals$days_to_close_clean, event = vx_deals$converted_event)

## NOTE: The model takes a LONG time to run, even on relatively small datasets.

# Run the RSF model
rsf_model <- ranger(
  formula = surv_obj ~ engagement_score + role + number_of_times_contacted + lifecycle_stage + industry,
  data = vx_deals,
  mtry = 3,
  num.trees = 1000,
  importance = "permutation",
  splitrule = "logrank",
  respect.unordered.factors = "partition"
)

# Extract variable importance table
importance_df <- data.frame(
  variable = names(rsf_model$variable.importance),
  importance = rsf_model$variable.importance
) %>%
  filter(!is.na(variable), !is.na(importance)) %>%
  filter(variable != "NA") %>%  # <-- This line is key
  arrange(desc(importance))


importance_df # ranked by how much the variables contribute to reducing prediction errors

importance_df_top10 <- importance_df %>% slice_max(order_by = importance, n = 10)

# Plot top variables
library(ggplot2)

# Plot top 10 variables by importance
ggplot(importance_df_top10, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "#2C7BB6") +
  coord_flip() +
  labs(
    title = "Significant Variables for Deal Velocity (RSF)",
    x = "Predictor",
    y = "Importance (permutation error)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )