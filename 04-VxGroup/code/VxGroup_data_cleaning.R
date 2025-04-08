library(here)
library(readr)
library(tidyverse)
library(janitor)
library(car)
library(ggplot2)
library(corrplot)
library(stringr)
library(stringdist)
here()

vx_deals <- read_csv(here("100-Projects", "04-VxGroup", "data_clean","all_deals.csv"))

## DATA CLEANING & PREPARATION ##

# Clean column names (snake_case)
vx_deals <- vx_deals %>% clean_names()

# View summary of missing values
missing_summary <- vx_deals %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "missing_pct") %>%
  arrange(desc(missing_pct))

print(missing_summary, n = 186)

# Remove columns with more than 99% missing values
cols_to_remove <- vx_deals %>% select(where(~ mean(is.na(.)) > 0.999))

vx_deals <- vx_deals %>% select(-all_of(colnames(cols_to_remove)))

# Trim whitespace from all character columns
vx_deals <- vx_deals %>% mutate(across(where(is.character), str_trim))

# Print clean dataset info
glimpse(vx_deals)

write_csv(vx_deals, here("100-Projects", "04-VxGroup", "data_clean", "all_deals_R.csv"))

##############################
## Prepare Contacts dataset ##
##############################

contacts <- read_csv(here("100-Projects", "04-VxGroup", "all-contacts-20250405-reconciled.csv"))

# Clean column names using snake_case
contacts <- contacts %>% clean_names()

# Trim whitespace from all character columns
contacts <- contacts %>% mutate(across(where(is.character), str_trim))

# Remove duplicate rows
contacts <- contacts %>% distinct()


# Function to simplify job titles
simplify_role <- function(title) {
  title <- tolower(title)

  case_when(
    is.na(title) ~ NA_character_,
    str_detect(title, "ceo|chief executive officer|founder|owner|president") ~ "CEO",
    str_detect(title, "managing director") ~ "Managing Director",
    str_detect(title, "executive director") ~ "Executive Director",
    str_detect(title, "vp|vice president") ~ "VP",
    str_detect(title, "\\bdirector\\b") ~ "Director",
    str_detect(title, "manager") ~ "Manager",
    str_detect(title, "coo|chief operating officer") ~ "COO",
    str_detect(title, "cfo|chief financial officer") ~ "CFO",
    str_detect(title, "cmo|chief marketing officer") ~ "CMO",
    str_detect(title, "chief revenue officer|cro") ~ "CRO",
    str_detect(title, "marketing director|director of marketing") ~ "Marketing Director",
    str_detect(title, "lead|head") ~ "Lead",
    str_detect(title, "engineer|developer|technician") ~ "Engineer",
    str_detect(title, "consultant|advisor|analyst") ~ "Consultant",
    str_detect(title, "cto|chief technology officer|vp of engineering|vice president of engineering") ~ "CTO",
    str_detect(title, "intern") ~ "Intern",
    TRUE ~ "Other"
  )
}

# Function to determine domain
determine_domain <- function(title) {
  title <- tolower(title)

  case_when(
    is.na(title) ~ NA_character_,
    str_detect(title, "marketing director|director of marketing") ~ "Marketing",
    str_detect(title, "ceo|chief executive officer|founder|president|owner|managing director|executive director") ~ "Executive",
    str_detect(title, "cmo|chief marketing officer|marketing|brand|growth|seo|content") ~ "Marketing",
    str_detect(title, "sales|account executive|business development|revenue|cro") ~ "Sales",
    str_detect(title, "finance|accounting|cfo|controller|bookkeeper") ~ "Finance",
    str_detect(title, "operations|coo|logistics|supply chain") ~ "Operations",
    str_detect(title, "product|ux|ui|design|designer") ~ "Product",
    str_detect(title, "human resources|hr|people|recruiter") ~ "HR",
    str_detect(title, "cto|engineer|developer|technician|technical") ~ "Engineering",
    str_detect(title, "customer success|support|client success|service") ~ "Customer Success",
    str_detect(title, "legal|attorney|lawyer|counsel") ~ "Legal",
    TRUE ~ "Other"
  )
}

# Apply the functions
contacts <- contacts %>%
  mutate(
    simplified_role = simplify_role(job_title_original),
    job_domain = determine_domain(job_title_original)
  )

##################################
## Prepare the company data set ##
##################################

vx_zoominfo <- read_csv(here("100-Projects", "04-VxGroup", "data_all", "vx_zoom_co_data.csv"))

vx_crm_co <- read_csv(here("100-Projects", "04-VxGroup", "data_all", "vx_company_list.csv"))

vx_zoominfo <- vx_zoominfo %>% clean_names()
vx_crm_co <- vx_crm_co %>% clean_names()

vx_zoominfo <- vx_zoominfo %>% distinct()
vx_crm_co <- vx_crm_co %>% distinct()

# Trim whitespace from all character columns
vx_zoominfo <- vx_zoominfo %>% mutate(across(where(is.character), str_trim))
vx_crm_co <- vx_crm_co %>% mutate(across(where(is.character), str_trim))

colnames(vx_zoominfo)
colnames(vx_crm_co)

# Company names were too messy to fully reconcile.
# Check company website urls in ZoomInfo and HubSpot (CRM) lists to see if they can be used as a primary key for the join

normalize_url <- function(url) {
  url <- tolower(url)
  url <- str_trim(url)
  url <- str_replace_all(url, "^https?://", "")  # remove http/https
  url <- str_replace(url, "^www\\.", "")         # remove www.
  url <- str_replace(url, "/$", "")              # remove trailing slash
  return(url)
}

# vx_zoominfo
# vx_crm_co

# Normalize website fields
crm_df <- vx_crm_co %>%
  mutate(website_norm = normalize_url(website_url))

zoominfo_df <- vx_zoominfo %>%
  mutate(website_norm = normalize_url(website))

# Join on normalized website
joined_df <- left_join(zoominfo_df, crm_df, by = "website_norm")

write_csv(joined_df, here("100-Projects", "04-VxGroup", "data_all", "joined_company_data.csv"))

# Check to see for how many companies we were able to match website urls and link the primary_company_id.
sum(is.na(joined_df$primary_company_id))
sum(!is.na(joined_df$primary_company_id))
sum(is.na(vx_crm_co$primary_company_id))
sum(!is.na(vx_crm_co$primary_company_id))

# I have primary_company_ids for 89% of the companies whose information exists in both the ZoomInfo list and the CRM.






