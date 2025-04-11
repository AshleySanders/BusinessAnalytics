library(here)
library(readr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(pheatmap)
library(FSA)
library(ggalt)

# Dataset: vx_fulljoin

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

kruskal.test(deal_amount ~ continent, data = vx_clean)
kruskal.test(closed_won_count ~ continent, data = vx_clean)
kruskal.test(days_to_close ~ continent, data = vx_clean)

# All results were significant, so we'll double check assumptions.
# First, we'll look at distributions :
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

ggplot(vx_summary_scaled,
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
    axis.text = element_text(size = 10)
  )

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




