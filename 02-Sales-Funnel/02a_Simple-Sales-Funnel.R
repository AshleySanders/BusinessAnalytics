library(here)
library(readxl)
library(tidyverse)
library(car)
library(ggplot2)
here()

simple_sales <- read_excel(here("r_tidy_ex", "data", "ecommerce", "sales-funnel.xlsx"))

# Set up the data needed to create the funnel visualization
simple_funnel <- as.data.frame(table(simple_sales$Status))
colnames(simple_funnel)[1] = "stage"
colnames(simple_funnel)[2] = "count"
simple_funnel

# You can use the geom_bar function to create the funnel. However, to get the funnel shape (wider at the top, narrower at the bottom), we can use the coord_flip() function to flip the bars horizontally and customize the widths.

ggplot(simple_funnel, aes(x = reorder(stage, count), y = count)) +
geom_bar(stat = "identity", fill = "skyblue", width = 0.7) + # bar chart
coord_flip() + # flip to horizontal bars
theme_minimal() + # clean theme
labs(
title = "Sales Funnel",
x = "Sales Stages",
y = "Number of Leads"
) +
theme(
axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
axis.text.y = element_text(size = 12),
plot.title = element_text(size = 16, face = "bold"),
axis.title.x = element_text(size = 14),
axis.title.y = element_text(size = 14))

# Polish the funnel visualization
ggplot(funnel_data, aes(x = reorder(stage, count), y = count)) +
geom_bar(stat = "identity", aes(fill = count), width = 0.7) +
coord_flip() +
scale_fill_gradient(low = "lightblue", high = "blue") +  # Color gradient
geom_text(aes(label = count), hjust = -0.3, size = 5) +  # Add text labels
theme_minimal() +
labs(
title = "Sales Funnel",
x = "Sales Stages",
y = "Number of Leads"
) +
theme(
axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
axis.text.y = element_text(size = 12),
plot.title = element_text(size = 16, face = "bold"),
axis.title.x = element_text(size = 14),
axis.title.y = element_text(size = 14)
)
ggplot(simple_funnel, aes(x = reorder(stage, count), y = count)) +
geom_bar(stat = "identity", aes(fill = count), width = 0.7) +
coord_flip() +
scale_fill_gradient(low = "lightblue", high = "blue") +  # Color gradient
geom_text(aes(label = count), hjust = -0.3, size = 5) +  # Add text labels
theme_minimal() +
labs(
title = "Sales Funnel",
x = "Sales Stages",
y = "Number of Leads"
) +
theme(
axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
axis.text.y = element_text(size = 12),
plot.title = element_text(size = 16, face = "bold"),
axis.title.x = element_text(size = 14),
axis.title.y = element_text(size = 14)
)

