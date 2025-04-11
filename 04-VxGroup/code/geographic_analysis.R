library(here)
library(readr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(pheatmap)
library(FSA)
library(ggalt)
library(dunn.test)
library(rcompanion)
library(effsize)

# Dataset: vx_fulljoin
vx_fulljoin <- read_csv(here("100-Projects", "04-VxGroup", "data_clean", "vx_fulljoin.csv"))

# Clean & transform the data
vx_clean <- vx_fulljoin %>%
  filter(
    !is.na(continent),
    !is.na(deal_amount),
    !is.na(closed_won_count),
    !is.na(days_to_close)
  ) %>%
  mutate(
    deal_amount = as.numeric(deal_amount),
    closed_won_count = as.numeric(closed_won_count),
    days_to_close = as.numeric(days_to_close)
  )

# Summarize by continent
region_summary <- vx_clean %>%
  group_by(continent) %>%
  summarise(
    total_deals = n(),
    closed_deals = sum(closed_won_count),
    avg_deal_size = mean(deal_amount, na.rm = TRUE),
    avg_days_to_close = mean(days_to_close, na.rm = TRUE),
    conversion_rate = closed_deals / total_deals,
    .groups = "drop"
  )

View(region_summary)

shapiro.test(vx_clean$deal_amount)
shapiro.test(vx_clean$closed_won_count)
shapiro.test(vx_clean$days_to_close)

# Data is non-normal as hypothesized, so proceeding with Kruskal-Wallis test
kruskal.test(deal_amount ~ continent, data = vx_clean)
kruskal.test(closed_won_count ~ continent, data = vx_clean)
kruskal.test(days_to_close ~ continent, data = vx_clean)

# All results were significant, so we'll double check assumptions.
# First, we'll look at distributions.
# Deal amount by continent
ggplot(vx_clean, aes(x = continent, y = deal_amount)) +
  geom_boxplot() +
  labs(title = "Deal Amount by Continent")

# Days to close
ggplot(vx_clean, aes(x = continent, y = days_to_close)) +
  geom_boxplot() +
  labs(title = "Days to Close by Continent")

# Conversion rate (note: use proportions, not count)
ggplot(vx_clean, aes(x = continent, y = closed_won_count)) +
  geom_boxplot() +
  labs(title = "Closed Deals Count by Continent")

# Although there are outliers, they are meaningful, so I'm keeping them in the data, and they won't effect the Kruskal-Wallis test results nor the post-hoc Dunn's Test I'll run next.

# Eta-Squared tells you the proportion of variance in the dependent variable (such as the deal amount or time to close) that can be explained by the independent variable (in this case, continent).

# How to interpret Eta-Squared Results:
# 0.01 (1%) = Small effect
# 0.06 (6%) = Medium effect
# 0.14 (14%) or more = Large effect

# Checking effect size:
# Ensure continent is a factor
vx_clean$continent <- as.factor(vx_clean$continent)

# Define Eta Squared effect size for Kruskal-Wallis Test
kwEtaSq <- function(kw_test) {
  H <- kw_test$statistic
  N <- sum(kw_test$parameter + 1)
  eta_sq <- as.numeric(H / (N - 1))
  return(eta_sq)
}

# Kruskal-Wallis + Eta-squared
kw_deal <- kruskal.test(deal_amount ~ continent, data = vx_clean)
kw_days <- kruskal.test(days_to_close ~ continent, data = vx_clean)

# Eta-squared estimates
eta_sq_deal <- kwEtaSq(kw_deal)
eta_sq_days <- kwEtaSq(kw_days)

cat("Eta-squared for Deal Amount:", eta_sq_deal, "\n")
cat("Eta-squared for Days to Close:", eta_sq_days, "\n")

# Large effect sizes shown for both deal amount and days to close, which means the continent has great explantory power for the differences observed.

# Check sample size per group before running post-hoc comparisons
table(vx_clean$continent)

# Post-hoc tests
# Deal Amount
dunnTest(deal_amount ~ continent, data = vx_clean, method = "bonferroni")
# The difference between North American and Oceania is significant, as are the differences between Asia & Insular Oceania, Africa & North America, Asia & North America, Europe and North America.

# Days to Close
dunnTest(days_to_close ~ continent, data = vx_clean, method = "bonferroni")

# Closed Won Count (Conversion proxy)
dunnTest(closed_won_count ~ continent, data = vx_clean, method = "bonferroni")

# Significant differences appear in each of the pairwise analyses and are summarized, along with their implications, in my notes.

# Next we'll look at effect size estimates for Dunn's Test.

# Function to compute pairwise Cliff's Delta
get_pairwise_cliffs <- function(data, metric, group_var) {
  group_levels <- unique(data[[group_var]])
  results <- data.frame(
    group1 = character(),
    group2 = character(),
    delta = numeric(),
    magnitude = character(),
    stringsAsFactors = FALSE
  )

  for (i in 1:(length(group_levels) - 1)) {
    for (j in (i + 1):length(group_levels)) {
      g1 <- group_levels[i]
      g2 <- group_levels[j]

      data1 <- data[[metric]][data[[group_var]] == g1]
      data2 <- data[[metric]][data[[group_var]] == g2]

      test <- cliff.delta(data1, data2)

      results <- rbind(results, data.frame(
        group1 = g1,
        group2 = g2,
        delta = test$estimate,
        magnitude = test$magnitude
      ))
    }
  }

  return(results)
}

# deal amount
cliffs_deal <- get_pairwise_cliffs(vx_clean, "deal_amount", "continent")
print(cliffs_deal)

#  days to close
cliffs_days <- get_pairwise_cliffs(vx_clean, "days_to_close", "continent")
print(cliffs_days)

# Combine and view
all_deltas <- rbind(cliffs_deal, cliffs_days)
print(all_deltas)

# DEAL VALUE: Large negative deltas between North America and Africa and Asia indicate that North America consistently has higher deal amounts. North America versus Europe and Oceania show moderate effect sizes, suggesting significantly better performance in deal value. Europe vs Asia shows a medium effect favoring Europe, and Europe versus South America shows a large effect favoring Europe once again.

# DAYS TO CLOSE: Large negative deltas between North America and Europe shows that North America closes significantly faster than Europe. North America is also much faster to close than Oceania and Insular Oceania with medium effect sizes, and Africa is slightly faster to clsoe than South America.

#---

# Summarize by continent
vx_summary <- vx_clean %>%
  group_by(continent) %>%
  summarise(
    avg_deal_amount = mean(deal_amount, na.rm = TRUE),
    avg_days_to_close = mean(days_to_close, na.rm = TRUE),
    closed_deal_count = sum(closed_won_count, na.rm = TRUE),
    .groups = "drop"
  )

## Create visualizations
# Lollipop Chart: Closed Deal Count by Continent

ggplot(vx_summary, aes(x = reorder(continent, closed_deal_count), y = closed_deal_count)) +
  geom_segment(aes(xend = continent, y = 0, yend = closed_deal_count), color = "gray") +
  geom_point(size = 4, color = "steelblue") +
  coord_flip() +
  labs(title = "Closed Deal Count by Continent", x = "Continent", y = "Closed Deals") +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Dumbbell Plot - Average Deal Amount vs Time to Close.
# Accessible colors used that meet at least the WCAG AA standards
# Put avg deal amount and avg days to close on the same scale


dumbbell_data <- vx_summary %>%
  mutate(
    scaled_deal_amount = scale(avg_deal_amount)[, 1],
    scaled_days_to_close = scale(avg_days_to_close)[, 1]
  )

vx_summary_scaled <- dumbbell_data %>%
  mutate(diff = scaled_deal_amount - scaled_days_to_close) %>%
  arrange(desc(diff))  # rank from highest (best performance) to lowest

dumbbell_plot <- ggplot(vx_summary_scaled,
       aes(y = reorder(continent, diff),
           x = scaled_deal_amount,
           xend = scaled_days_to_close)) +
  geom_dumbbell(color = "#4E4E4E",
                size = 3,
                size_x = 5,
                size_xend = 5,
                colour_x = "#0072B2",
                colour_xend = "#D55E00") +
  labs(
    title = "Performance by Continent: Deal Value vs Time to Close (Standardized)",
    subtitle = "Ranked by high deal value and low time to close",
    x = "Standardized Value (z-score)",
    y = "Continent",
    caption = "Blue = Standardized Avg Deal Amount | Orange = Standardized Avg Days to Close"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 10),
    plot.margin = unit(c(2, 1, 1, 1), "cm")
  )

dumbbell_plot
ggsave(here("100-Projects", "04-VxGroup", "output", "continent_performance_hd.png"),
       plot = dumbbell_plot,
       width = 12, height = 7,  # Slightly larger dimensions
       dpi = 600,               # Higher resolution
       device = "png")

ggsave(here("100-Projects", "04-VxGroup", "output", "continent_performance.svg"),
       plot = dumbbell_plot,
       width = 10, height = 6,
       device = "svg")

# Simulate Dunn test result frame
dunn_df <- dunnTest(deal_amount ~ continent, data = vx_clean, method = "bonferroni")$res

# Filter significant comparisons
sig_dunn <- dunn_df %>%
  filter(P.adj < 0.05) %>%
  mutate(
    group1 = str_trim(str_split_fixed(Comparison, "-", 2)[,1]),
    group2 = str_trim(str_split_fixed(Comparison, "-", 2)[,2])
  )

# Heatmap data prep
heatmap_data <- sig_dunn %>%
  select(group1, group2, P.adj) %>%
  pivot_wider(names_from = group2, values_from = P.adj, values_fill = 1)

# Turn into matrix
heatmap_matrix <- as.matrix(heatmap_data[,-1])
rownames(heatmap_matrix) <- heatmap_data$group1

# Heatmap
pheatmap(heatmap_matrix,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         display_numbers = TRUE,
         main = "Significant Pairwise Differences in Deal Amounts",
         color = colorRampPalette(c("#D55E00", "white"))(100))




