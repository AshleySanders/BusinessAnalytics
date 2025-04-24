

monthly_data <- monthly_data %>% clean_names()

# Adjusted blended forecast to reduce influence of Prophet model, which is overly optimistic in months when there are few deals overall and no high-tier leads, and then prioritizes this model when there's enough signal in the data to justify the optimism

blended_forecast_adjusted <- comparison_df %>%
  left_join(monthly_data, by = "ds") %>%
  mutate(
    lead_share = high_tier_lead_count / total_deals,
    prophet_xgb_gap = abs(prophet_yhat - xgb_yhat),

    weight_xgb = case_when(
      total_deals < 3 ~ 0.9,                                # Very low deal volume
      high_tier_lead_count == 0 ~ 0.85,                          # No high quality leads
      prophet_xgb_gap > 200000 ~ 0.7,                       # Large divergence
      lead_share < 0.3 ~ 0.6,                               # Mostly low quality leads
      lead_share > 0.7 ~ 0.4,                               # Mostly high quality leads
      TRUE ~ 0.5                                            # Balanced case
    ),

    weight_prophet = 1 - weight_xgb,

    blended_yhat = xgb_yhat * weight_xgb + prophet_yhat * weight_prophet,
    blended_lower = xgb_lower * weight_xgb + prophet_lower * weight_prophet,
    blended_upper = xgb_upper * weight_xgb + prophet_upper * weight_prophet
  ) %>%
  select(
    ds, blended_yhat, blended_lower, blended_upper,
    xgb_yhat, prophet_yhat, weight_xgb, weight_prophet,
    high_tier_lead_count, total_deals
  )


View(blended_forecast_adjusted)

write_csv(blended_forecast_adjusted, here("100-Projects", "04-VxGroup", "output", "blended_forecast_adjusted.csv"))
