# Load essential libraries

library(here)
library(readr)
library(tidyverse)
library(car)
library(ggplot2)
library(corrplot)
here()

# About: This project explores marketing and sales funnel data from Olist, a Brazilian online retailer

# Import Data
mql <- read_csv(here("r_tidy_ex","data", "tidy_data", "olist_marketing_qualified_leads_dataset.csv"))
closed <- read_csv(here("r_tidy_ex", "data", "tidy_data", "olist_closed_deals_dataset.csv"))

# Join two data sets on Marketing Qualified Leads ID (mql_id)
olist <- left_join(mql, closed, by = "mql_id", copy = FALSE)
View(olist)

# Examine the number of closed deals by lead source (origin)

olist <- olist %>% mutate(origin = ifelse(is.na(origin), "Missing", origin))

glimpse(olist)

lead_source <- sort(table(olist$origin), decreasing=TRUE)

lead_source
barplot(lead_source)


## Calculate Funnel Stage Counts and Conversion/Drop-off Rates

# Calculate the number of leads at each funnel stage
total_mql <- nrow(olist) # Total MQLs
total_sql <- olist %>% filter(!is.na(sdr_id)) %>% nrow() # Leads that become SQLs (have sdr_id)
total_pres <- olist %>%  filter(!is.na(sr_id)) %>% nrow() # Leads that have a presentation (have sr_id)
total_won <- olist %>% filter(!is.na(won_date)) %>% nrow() # Leads that closed (have dates for won_date)

# Calculate conversion rates between stages
cvr_mql_sql <- total_sql/total_mql # MQL -> SQL conversion rate
cvr_sql_pres <- ifelse(total_sql > 0, total_pres / total_sql, 0) # SQL -> Presentation
cvr_pres_won <- ifelse(total_pres > 0, total_won / total_pres, 0) # Presentation -> Won

# Calculate drop-off rates between stages (complement of conversion)
drop_mql_sql <- 1 - cvr_mql_sql # Drop-off from MQL before reaching SQL
drop_sql_pres <- 1 - cvr_sql_pres # Drop-off from SQL before Presentation
drop_pres_won <- 1 - cvr_pres_won # Drop-off from Presentation before Won

# Create a summary tibble for funnel stages
funnel_summary <- tibble(
  Stage = factor(c("MQL", "SQL", "Presentation", "Won"),
                 levels = c("Won", "Presentation", "SQL", "MQL")), #factor levels set to have MQL at top when plotting
  Leads = c(total_mql, total_sql, total_pres, total_won),
  Conversion_rate = c(NA, cvr_mql_sql, cvr_sql_pres, cvr_pres_won),
  Drop_off_rate = c(NA, drop_mql_sql, drop_sql_pres, drop_pres_won)
)

# Add percentage strings for easier reading (Conversion/Drop-off between each stage)
funnel_summary <- funnel_summary %>%
  mutate(
    Conversion_pct = ifelse(is.na(Conversion_rate), "N/A", paste0(round(Conversion_rate * 100, 1), "%")),
    Drop_off_pct = ifelse(is.na(Drop_off_rate), "N/A", paste0(round(Drop_off_rate * 100, 1), "%"))
  )

# Print the funnel summary table with the conversion and drop-off rates
cat("\n**Funnel Summary (Lead counts with conversion and drop-off rates):**\n")
print(funnel_summary %>% select(Stage, Leads, Conversion_pct, Drop_off_pct), n=Inf)

## Funnel Conversion Chart (MQL → SQL → Presentation → Won) ##

# Prepare labels for conversion rates between stages to annotate on the chart
conv_labels <- tibble(
  label = c(paste0(round(cvr_mql_sql*100, 1), "%"),
            paste0(round(cvr_sql_pres*100, 1), "%"),
            paste0(round(cvr_pres_won*100, 1), "%")),
  # Position the labels at midpoints between stages on the y-axis
  y = c(4, 2.5, 1.5),  # midpoints between MQL-SQL, SQL-Pres, Pres-Won (given factor levels ordering)
  # Position labels at the midpoint of the two bar lengths on the x-axis
  x = c((total_mql + total_sql)/2,
        (total_sql + total_pres)/2,
        (total_pres + total_won)/2)
)

# Plot the funnel chart
ggplot(funnel_summary, aes(x = Leads, y = Stage)) +
  geom_col(fill = "skyblue", color = "black") +
  geom_text(data = conv_labels, aes(x = x, y = y, label = label),
            fontface = "bold", color = "black", size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +  # small expansion to accommodate labels at ends
  labs(title = "Sales Funnel: Lead Volume by Stage",
       x = "Number of Leads", y = "Funnel Stage") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


### Conversion Rate by Lead Origin ###

# Group leads by origin and calculate the total leads, won deals, and conversion rates for each origin.
origin_summary <- olist %>%
  group_by(origin) %>%
  summarise(
    Leads = n(),
    Won_deals = sum(!is.na(won_date))
  ) %>%
  mutate(Conversion_rate = ifelse(Leads > 0, Won_deals/Leads, 0)) %>%
  ungroup() %>%
  arrange(desc(Conversion_rate))

# Create a conversion percentage label for clarity
origin_summary <- origin_summary %>%
  mutate(Conversion_pct = paste0(round(Conversion_rate*100, 1), "%"))

# Print the conversion by origin summary table
cat("\n**Conversion by Lead Origin (sorted by covnersion rate):**\n")
print(origin_summary %>% select(origin, Leads, Won_deals, Conversion_pct), n = Inf)


origins_table <- within(origin_summary, origin <- factor(origin, levels = origin))
origins_table

# Plot a bar chart of conversion rates by origin (highest to lowest)
ggplot(origins_table, aes(x = Conversion_rate, y = origin)) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Lead Conversion Rate by Origin",
       x = "Conversion Rate (Won Deals / Total Leads)",
       y = "Lead Origin") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


## Are there specific lead characteristics that strongly correlate with conversion success? ##

# Create a binary conversion column
olist <- olist %>%
  mutate(converted = ifelse(!is.na(won_date), 1, 0))

### Function to summarize and plot characteristics ###
analyze_conversion <- function(data, characteristic, top_n = 10) {
  summary <- data %>%
    filter(!is.na(.data[[characteristic]])) %>%
    group_by(.data[[characteristic]]) %>%
    summarise(
      total_leads = n(),
      conversions = sum(converted),
      conversion_rate = mean(converted)
    ) %>%
    arrange(desc(conversion_rate))

  # Top categories by volume
  top_summary <- summary %>% top_n(top_n, wt = total_leads)

  # Plot
  plot <- ggplot(top_summary, aes(x = reorder(.data[[characteristic]], total_leads), y = total_leads)) +
    geom_col(fill = "skyblue") +
    coord_flip() +
    labs(title = paste("Conversion Rate by", characteristic, "(Top", top_n, "by volume)"),
         x = characteristic,
         y = "Conversion Rate") +
    theme_minimal()

  print(plot)
  return(summary)
}

# Since all of the SQL conversion rates are 100%, the following is not illuminating, but it's good practice, nevertheless, since this type of analysis is normally much more meaningful.

# Analyze and visualize each characteristic (other than lead origin)
# 1. Business Segment
business_segment_summary <- analyze_conversion(olist, "business_segment")

# 2. Lead Type
lead_type_summary <- analyze_conversion(olist, "lead_type")

# 3. Lead Behaviour Profile
lead_behaviour_summary <- analyze_conversion(olist, "lead_behaviour_profile")

# 4. Business Type
business_type_summary <- analyze_conversion(olist, "business_type")

### Print Summaries ###
print("Conversion by Lead Origin:")
print(origin_summary)

print("Conversion by Business Segment:")
print(business_segment_summary)

print("Conversion by Lead Type:")
print(lead_type_summary)

print("Conversion by Lead Behaviour Profile:")
print(lead_behaviour_summary)

print("Conversion by Business Type:")
print(business_type_summary)

## Examining potential relationships between variables of leads who convert:

library(vcd)          # for mosaic plots
library(ggmosaic)     # alternative for mosaic plots
library(gridExtra)    # arrange multiple plots

# define a reusable function for fisher exact tests
analyze_relationship <- function(df, var1, var2) {
  # Remove NA values from both variables
  temp_df <- df %>% select(all_of(c(var1, var2))) %>% drop_na()

  # Fisher Exact test
  fisher <- fisher.test(table(temp_df[[var1]], temp_df[[var2]]), simulate.p.value=TRUE)

  cat("\nFisher's Exact test between", var1, "and", var2, ":\n")
  print(fisher)

  return(fisher)
}

# Perform Chi-square tests and Create Mosaic Plots
# Origin vs Business Segment
fisher_origin_segment <- analyze_relationship(olist, "origin", "business_segment")

# Origin vs Lead Type
fisher_origin_leadtype <- analyze_relationship(olist, "origin", "lead_type")

# Origin vs Lead Behaviour Profile
fisher_origin_behaviour <- analyze_relationship(olist, "origin", "lead_behaviour_profile")

# Origin vs Business Type
fisher_origin_businesstype <- analyze_relationship(olist, "origin", "business_type")

## All the variables are independent. There are no patterns of correlation between them.

## Logistic regression to identify factors that are useful in predicting conversion
library(stats)
logit_model <- glm(converted ~ origin + business_segment + lead_type + lead_behaviour_profile + business_type,
                   data=olist, family=binomial())

# The logistic regression encountered a "Perfect separation detected" error. This happens when one or more predictors perfectly predict the outcome, causing the logistic regression model to fail because coefficients become infinite or undefined. See comment below for explanation as to why this occurred.

# A decision tree can't be run because we don't have any information regarding these characteristics for the leads who did not convert. This limitation in the data severely limits our ability to analyze the factors that lead to conversion.
