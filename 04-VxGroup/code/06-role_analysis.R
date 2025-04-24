library(here)
library(readr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(scales)

# Full funnel engagement analysis by role.

glimpse(engagement)

engagement_by_role <- engagement %>%
  group_by(role) %>%
  summarise(
    avg_engagement_score = mean(engagement_score_raw, na.rm = TRUE),
    avg_pageviews = mean(number_of_pageviews, na.rm = TRUE),
    avg_sessions = mean(number_of_sessions, na.rm = TRUE),
    avg_form_submissions = mean(number_of_form_submissions, na.rm = TRUE),
    avg_email_opens = mean(marketing_emails_opened, na.rm = TRUE),
    avg_email_clicks = mean(marketing_emails_clicked, na.rm = TRUE),
    avg_email_replies = mean(marketing_emails_replied, na.rm = TRUE),
    avg_sales_email_replied = mean(recent_sales_email_replied, na.rm = TRUE),
    avg_currently_in_sequency = mean(currently_in_sequence, na.rm = TRUE),
    contact_count = n()
  ) %>%
  arrange(desc(avg_engagement_score))

# View table
print(engagement_by_role)
View(engagement_by_role)

# Bottom of funnel analysis engagement analysis
engagement_unique <- engagement %>%
  distinct(contact_id, .keep_all = TRUE)

glimpse(engagement_unique)

deals_contacts <- left_join(deals, engagement_unique, by = "contact_id", relationship = "many-to-one") %>% clean_names()

engagement_deals_by_role <- deals_contacts %>%
  group_by(role) %>%
  summarise(
    avg_engagement_score = mean(engagement_score_raw, na.rm = TRUE),
    avg_pageviews = mean(number_of_pageviews, na.rm = TRUE),
    avg_sessions = mean(number_of_sessions, na.rm = TRUE),
    avg_form_submissions = mean(number_of_form_submissions, na.rm = TRUE),
    avg_email_opens = mean(marketing_emails_opened, na.rm = TRUE),
    avg_email_clicks = mean(marketing_emails_clicked, na.rm = TRUE),
    avg_email_replies = mean(marketing_emails_replied, na.rm = TRUE),
    avg_sales_email_replied = mean(recent_sales_email_replied, na.rm = TRUE),
    avg_currently_in_sequence = mean(currently_in_sequence, na.rm = TRUE),
    contact_count = n()
  ) %>%
  arrange(desc(avg_engagement_score))

print(engagement_deals_by_role)
View(engagement_deals_by_role)

# Comparison analysis

# Filter engaged contacts from full contacts dataset
engaged_contacts_all <- engagement %>%
  filter(engagement_score_raw > 0) %>%
  group_by(role) %>%
  summarise(n_all = n(), .groups = "drop") %>%
  mutate(percent_all = n_all / sum(n_all))

engaged_contacts_all # View results

# Filter engaged contacts tied to deal
engaged_contacts_deals <- deals_contacts %>%
  filter(engagement_score_raw > 0) %>%
  group_by(role) %>%
  summarise(n_deals = n(), .groups = "drop") %>%
  mutate(percent_deals = n_deals / sum(n_deals))

engaged_contacts_deals

# Join and compare role breakdowns
role_comparison <- engaged_contacts_all %>%
  full_join(engaged_contacts_deals, by = "role") %>%
  replace_na(list(n_all = 0, percent_all = 0, n_deals = 0, percent_deals = 0)) %>%
  mutate(
    percent_diff = percent_deals - percent_all,
    percent_diff_label = percent_diff %>% percent(accuracy = 0.1)
  ) %>%
  arrange(desc(abs(percent_diff)))

print(role_comparison)
View(role_comparison)

# Plot role differences
ggplot(role_comparison, aes(x = reorder(role, percent_diff), y = percent_diff)) +
  geom_col(fill = ifelse(role_comparison$percent_diff > 0, "#00BFC4", "#F8766D")) +
  geom_text(aes(label = percent_diff_label), hjust = ifelse(role_comparison$percent_diff > 0, -0.1, 1.1)) +
  coord_flip() +
  labs(
    title = "Role Representation: Engaged Contacts vs Deals",
    subtitle = "Positive = Overrepresented in deals; Negative = Drop-off from initial engagement",
    x = "Role",
    y = "Difference in Proportion"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Statistical analysis of engagement differences by role

# Create a contingency table of role by group
library(tidyverse)

# Updated contingency table creation
role_counts <- bind_rows(
  engagement %>%
    filter(engagement_score_raw > 0, !is.na(role)) %>%  # Remove NAs
    mutate(group = "All_Engaged") %>%
    select(role, group),

  deals_contacts %>%
    filter(engagement_score_raw > 0, !is.na(role)) %>%  # Remove NAs
    mutate(group = "Deals_Engaged") %>%
    select(role, group)
) %>%
  count(group, role) %>%
  pivot_wider(names_from = group, values_from = n, values_fill = 0) %>%
  filter(!is.na(role)) %>%  # Redundant safeguard
  column_to_rownames("role")

# View resulting contingency table
print(role_counts)

# Run fisher's exact test
fisher_test <- fisher.test(role_counts, simulate.p.value = TRUE)

fisher_test
# p = 0.0005 *** statistically significant difference in role distributions between engaged contacts and deal-engaged contacts.

# Pairwise post-hoc analysis with p-value adjustment for multiple tests

# Reshape data back into long format for iteration
role_long <- role_counts %>%
  rownames_to_column("role") %>%
  pivot_longer(cols = c("All_Engaged", "Deals_Engaged"), names_to = "group", values_to = "count") %>%
  pivot_wider(names_from = group, values_from = count)

# Create a list to store results
posthoc_results <- purrr::map_dfr(1:nrow(role_long), function(i) {
  row <- role_long[i, ]

  mat <- matrix(
    c(row$Deals_Engaged, sum(role_long$Deals_Engaged) - row$Deals_Engaged,
      row$All_Engaged, sum(role_long$All_Engaged) - row$All_Engaged),
    nrow = 2,
    byrow = TRUE
  )

  test <- fisher.test(mat)

  tibble(
    role = row$role,
    p_value = test$p.value
  )
}) %>%
  mutate(p_adj = p.adjust(p_value, method = "holm"))  # Adjust for multiple comparisons

posthoc_results

# Merge test results with role_comparison
role_comparison_sig <- role_comparison %>%
  left_join(posthoc_results, by = "role") %>%
  arrange(desc(abs(percent_diff)))

write_csv(role_comparison_sig, here("100-Projects", "04-VxGroup", "output", "role_comparison_sig.csv"))

# Funnel visualization

# Create a table of comparison proportions and sort roles by percent_all for funnel appearance
role_funnel <- role_comparison %>%
  select(role, n_all, percent_all, percent_deals) %>%
  filter(!is.na(role)) %>%
  filter(n_all >= 100) %>%
  arrange(desc(percent_all)) %>%
  mutate(y = row_number()) # Create y-levels from top to bottom

# Function to create each trapezoid (clockwise points)
polygon_data <- role_funnel %>%
  mutate(
    x_left_top = -percent_all / 2,
    x_right_top = percent_all / 2,
    x_right_bottom = percent_deals / 2,
    x_left_bottom = -percent_deals / 2,
    y_top = y + 0.4,
    y_bottom = y - 0.4
  ) %>%
  rowwise() %>%
  mutate(
    x = list(c(x_left_top, x_right_top, x_right_bottom, x_left_bottom)),
    y = list(c(y_top, y_top, y_bottom, y_bottom))
  ) %>%
  unnest(c(x, y))

# Create annotation labels for top and bottom
annotations <- tibble::tibble(
  x = c(0, 0),
  y = c(max(role_funnel$y) + 0.8, min(role_funnel$y) - 0.8),
  label = c("All Engaged Contacts", "Contacts with Deals")
)

# Plot using ggplot2
ggplot(polygon_data, aes(x = x, y = y, group = role, fill = role)) +
  geom_polygon(color = "white", alpha = 0.9) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(
    breaks = role_funnel$y,
    labels = role_funnel$role,
    expand = expansion(add = 0.5)
  ) +
  scale_x_continuous(labels = percent_format()) +
  geom_text(data = annotations, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, fontface = "bold", size = 4.2) +
  labs(
    title = "Funnel of Engagement by Role",
    subtitle = "Drop-off from All Engaged Contacts to Contacts with Deals",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none"
  )

