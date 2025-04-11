# Load necessary libraries
library(tidyverse)
library(scales)  # for percent formatting
library(ggplot2)
library(ggiraph)
library(htmlwidgets)
library(here)
here()

# Data sets:
# vx_deals = cleaned and simplified deals data set downloaded from HubSpot
# companies_unique = cleaned and enriched dataset from ZoomInfo with HubSpot primary_company_ids; reduced to a list of unique primary_company_ids
# vx_fulljoin = a left many-to-one join between vx_deals and companies_unique
# vx_fulljoin = a left many-to-one join between vx_fulljoin and contacts_unique.

#######################
## Conversion Rates ##
######################

# Explore conversion rates by job title (called role in the dataset) and domain.


data_clean <- vx_fulljoin %>%
  filter(!is.na(role), !is.na(job_domain))

# Summarize by role and domain
summary_df <- data_clean %>%
  group_by(role, job_domain) %>%
  summarise(
    total_deals = n(),
    closed_deals = sum(closed_won_count),
    conversion_rate = closed_deals / total_deals,
    .groups = 'drop'
  )

# Calculate width (role share of total deals)
role_totals <- summary_df %>%
  group_by(role) %>%
  summarise(
    role_total_deals = sum(total_deals),
    .group = 'drop'
  ) %>%
  mutate(
    role_width = role_total_deals / sum(role_total_deals)
  )

# Merge widths back to summary_df
mosaic_df <- summary_df %>%
  left_join(role_totals, by = 'role')

# Prepare summary data
mosaic_data <- vx_fulljoin %>%
  filter(!is.na(role), !is.na(job_domain)) %>%
  group_by(role, job_domain) %>%
  summarise(
    n_deals = n(),
    n_closed = sum(closed_won_count, na.rm = TRUE),
    conversion_rate = n_closed / n_deals,
    .groups = "drop"
  ) %>%
  group_by(role) %>%
  mutate(
    total_deals_role = sum(n_deals),
    domain_prop = n_deals / total_deals_role
  ) %>%
  ungroup()

# Calculate role widths
role_widths <- mosaic_data %>%
  group_by(role) %>%
  summarise(width = sum(n_deals), .groups = "drop") %>%
  mutate(width = width / sum(width)) %>%
  arrange(desc(width)) %>%
  mutate(
    xmin = lag(cumsum(width), default = 0),
    xmax = xmin + width
  )

# Add positions
mosaic_data <- mosaic_data %>%
  left_join(role_widths, by = "role") %>%
  group_by(role) %>%
  arrange(job_domain, .by_group = TRUE) %>%
  mutate(
    ymin = lag(cumsum(domain_prop), default = 0),
    ymax = ymin + domain_prop
  ) %>%
  ungroup()

mosaic_data <- mosaic_data %>%
  mutate(
    tooltip_text = paste0(
      "Role: ", role, "\n",
      "Domain: ", job_domain, "\n",
      "Deals: ", n_deals, "\n",
      "Conversion Rate: ", round(conversion_rate * 100, 1), "%"
    )
  )

p <- ggplot(mosaic_data) +
  geom_rect_interactive(
    aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax,
      fill = conversion_rate,
      tooltip = tooltip_text
    ),
    color = "white"
  ) +
  scale_fill_viridis_c(name = "Conversion Rate", labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "right")

# Create girafe object
mosaic_girafe <- girafe(
  ggobj = p,
  width_svg = 10,
  height_svg = 6,
  options = list(
    opts_tooltip(opacity = 0.95, css = "padding:6px; background-color:#333; color:white; border-radius:4px;"),
    opts_hover(css = "stroke:black;stroke-width:2px;")
  )
)

# Save to HTML file
saveWidget(mosaic_girafe, file = "conversion_mosaic_plot.html", selfcontained = TRUE)


