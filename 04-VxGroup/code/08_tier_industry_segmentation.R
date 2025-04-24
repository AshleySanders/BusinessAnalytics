library(dplyr)
library(tidyr)
library(janitor)
library(forcats)
library(ggplot2)

# Use deal_amount as actual outcome
historical_data <- historical_data %>%
  mutate(
    expected_deal_value = deal_amount,
    lead_score_tier_xgb = fct_na_value_to_level(as.factor(lead_score_tier_xgb), level = "Low"),
    primary_industry = fct_na_value_to_level(as.factor(primary_industry), level = "unknown"))

# Fisher's Exact Test: Tier vs. Industry
# Contingency table
hist_tier_industry_table <- historical_data %>%
  count(lead_score_tier_xgb, primary_industry) %>%
  pivot_wider(names_from = primary_industry, values_from = n, values_fill = 0)

# Run chi-square test
hist_chi_matrix <- hist_tier_industry_table %>%
  select(-lead_score_tier_xgb) %>%
  as.matrix()

hist_chi_result <- chisq.test(hist_chi_matrix) # approximation may be incorrect

hist_fishers_test <- fisher.test(hist_chi_matrix, simulate.p.value = TRUE)

hist_fishers_test # not significant

##------------------------------------------------------------------------------

## HISTORICAL SUMMARY TABLES

# A. By Industry
industry_summary_hist <- historical_data %>%
  group_by(primary_industry) %>%
  summarise(
    total_actual_sales = sum(expected_deal_value, na.rm = TRUE),
    avg_actual_deal = mean(expected_deal_value, na.rm = TRUE),
    n_deals = n()
  ) %>%
  arrange(desc(total_actual_sales))

industry_summary_hist

# B. By Lead Score Tier
tier_summary_hist <- historical_data %>%
  group_by(lead_score_tier_xgb) %>%
  summarise(
    total_actual_sales = sum(expected_deal_value, na.rm = TRUE),
    avg_actual_deal = mean(expected_deal_value, na.rm = TRUE),
    n_deals = n()
  ) %>%
  arrange(desc(total_actual_sales))

tier_summary_hist

# C. By Tier + Industry
tier_industry_summary_hist <- historical_data %>%
  group_by(lead_score_tier_xgb, primary_industry) %>%
  summarise(
    total_actual_sales = sum(expected_deal_value, na.rm = TRUE),
    avg_actual_deal = mean(expected_deal_value, na.rm = TRUE),
    n_deals = n()
  ) %>%
  arrange(desc(total_actual_sales))

tier_industry_summary_hist

##------------------------------------------------------------------------------

## SEGMENTING FORECASTS

# 1. Create a contingency table: Lead Score Tier vs. Industry
tier_industry_table <- forecast_data %>%
  count(lead_score_tier_xgb, primary_industry) %>%
  pivot_wider(names_from = primary_industry, values_from = n, values_fill = 0)

# 2. Chi-square test
# We'll need to convert to a matrix and remove the tier column for the test
chi_matrix <- tier_industry_table %>% select(-lead_score_tier_xgb) %>% as.matrix()
chi_result <- chisq.test(chi_matrix) # may be incorrect => going to use Fisher's exact test

fisher.test(chi_matrix) # significant

# 3. Aggregate deal value by tier and industry
# First ensure expected deal value column
forecast_data <- forecast_data %>%
  mutate(expected_deal_value = predicted_deal_amount * predicted_prob_xgb)

# A. By industry
industry_summary <- forecast_data %>%
  group_by(primary_industry) %>%
  summarise(
    total_expected_sales = sum(expected_deal_value, na.rm = TRUE),
    avg_expected_deal = mean(expected_deal_value, na.rm = TRUE),
    n_deals = n()
  ) %>%
  arrange(desc(total_expected_sales))

industry_summary

# B. By tier
tier_summary <- forecast_data %>%
  group_by(lead_score_tier_xgb) %>%
  summarise(
    total_expected_sales = sum(expected_deal_value, na.rm = TRUE),
    avg_expected_deal = mean(expected_deal_value, na.rm = TRUE),
    n_deals = n()
  ) %>%
  arrange(desc(total_expected_sales))

tier_summary

# C. By tier + industry
tier_industry_summary <- forecast_data %>%
  group_by(lead_score_tier_xgb, primary_industry) %>%
  summarise(
    total_expected_sales = sum(expected_deal_value, na.rm = TRUE),
    avg_expected_deal = mean(expected_deal_value, na.rm = TRUE),
    n_deals = n()
  ) %>%
  arrange(desc(total_expected_sales))

tier_industry_summary

##------------------------------------------------------------------------------

## Plot comparisons

# Add a source label
industry_summary_hist <- industry_summary_hist %>%
  mutate(source = "Historical") %>%
  rename(total_sales = total_actual_sales)

industry_summary_forecast <- industry_summary %>%
  mutate(source = "Forecast") %>%
  rename(total_sales = total_expected_sales)

# Combine
industry_comparison <- bind_rows(industry_summary_hist, industry_summary_forecast)

# Bar chart: Total Sales by Industry & Source
ggplot(industry_comparison, aes(x = fct_reorder(primary_industry, total_sales), y = total_sales, fill = source)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Total Sales by Industry (Forecast vs Historical)",
       x = "Industry",
       y = "Total Sales",
       fill = "Data Source") +
  theme_minimal()

# Bar chart: Total Sales by Industry & Source
ggplot(industry_comparison, aes(x = fct_reorder(primary_industry, total_sales), y = total_sales, fill = source)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Total Sales by Industry (Forecast vs Historical)",
       x = "Industry",
       y = "Total Sales",
       fill = "Data Source") +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# [Fix discrepancy between variable names in forecast and historical summaries] Bar chart: Average Deal Value by Industry & Source
ggplot(industry_comparison, aes(x = fct_reorder(primary_industry, avg_actual_deal), y = avg_actual_deal, fill = source)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Total Sales by Industry (Forecast vs Historical)",
       x = "Industry",
       y = "Average Deal Value",
       fill = "Data Source") +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Faceted Chart: Industry x Tier

# Prepare for faceted plot
tier_industry_summary_hist <- tier_industry_summary_hist %>%
  mutate(source = "Historical") %>%
  rename(total_sales = total_actual_sales)

tier_industry_summary_forecast <- tier_industry_summary %>%
  mutate(source = "Forecast") %>%
  rename(total_sales = total_expected_sales)

tier_industry_combined <- bind_rows(tier_industry_summary_hist, tier_industry_summary_forecast)

# Plot
ggplot(tier_industry_combined, aes(x = fct_reorder(primary_industry, total_sales), y = total_sales, fill = source)) +
  geom_col(position = "dodge") +
  facet_wrap(~ lead_score_tier_xgb) +
  coord_flip() +
  labs(title = "Sales by Industry and Tier (Forecast vs Historical)",
       x = "Industry",
       y = "Total Sales",
       fill = "Data Source") +
  theme_minimal()
