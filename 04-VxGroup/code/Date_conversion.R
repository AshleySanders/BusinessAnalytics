# DATES

# Convert dates and numerics if needed (example)
vx <- vx %>%
  mutate(close_date = as.Date(close_date, format = "%Y-%m-%d"),
         create_date = as.Date(create_date, format = "%Y-%m-%d"),
         first_contact_create_date = as.Date(first_contact_create_date, format = "%Y-%m-%d"),
         first_conversion_date = as.Date(first_conversion_date, format = "%Y-%m-%d"),
         last_activity_date = as.Date(last_activity_date, format = "%Y-%m-%d"),
         last_engagement_date = as.Date(last_engagement_date, format = "%Y-%m-%d"),
         last_logged_outgoing_email_date = as.Date(last_logged_outgoing_email_date, format = "%Y-%m-%d"),
         last_modified_date = as.Date(last_modified_date, format = "%Y-%m-%d"),
         latest_traffic_source_timestamp = as.Date(latest_traffic_source_timestamp, format = "%Y-%m-%d"),
         marketing_qualified_date = as.Date(marketing_qualified_date, format = "%Y-%m-%d"),
         owner_assigned_date = as.Date(owner_assigned_date, format = "%Y-%m-%d"),
         recent_conversion_date = as.Date(recent_conversion_date, format = "%Y-%m-%d"),
         recent_deal_close_date = as.Date(recent_deal_close_date, format = "%Y-%m-%d"),
         time_first_seen = as.Date(time_first_seen, format = "%Y-%m-%d"),
         year_founded = as.integer(year_founded),
         annual_revenue = as.numeric(annual_revenue))

closed <- closed %>%
  mutate(x1st_meeting_booked_date = as.Date(x1st_meeting_booked_date, format = "%Y-%m-%d"),
         x1st_meeting_date = as.Date(x1st_meeting_date, format = "%Y-%m-%d"),
         close_date = as.Date(close_date, format = "%Y-%m-%d"),
         create_date = as.Date(create_date, format = "%Y-%m-%d"),
         first_conversion_date = as.Date(first_conversion_date, format = "%Y-%m-%d"),
         first_deal_created_date = as.Date(first_deal_created_date, format = "%Y-%m-%d"),
         last_activity_date = as.Date(last_activity_date, format = "%Y-%m-%d"),
         last_engagement_date = as.Date(last_engagement_date, format = "%Y-%m-%d"),
         last_modified_date = as.Date(last_modified_date, format = "%Y-%m-%d"),
         last_sequence_ended_date = as.Date(last_sequence_ended_date, format = "%Y-%m-%d"),
         last_sequence_enrolled_date = as.Date(last_sequence_enrolled_date, format = "%Y-%m-%d"),
         latest_traffic_source_date = as.Date(latest_traffic_source_date, format = "%Y-%m-%d"),
         next_activity_date = as.Date(next_activity_date, format = "%Y-%m-%d"),
         owner_assigned_date = as.Date(owner_assigned_date, format = "%Y-%m-%d"),
         recent_conversion_date = as.Date(recent_conversion_date, format = "%Y-%m-%d"),
         recent_deal_close_date = as.Date(recent_deal_close_date, format = "%Y-%m-%d"),
         recent_sales_email_clicked_date = as.Date(recent_sales_email_clicked_date, format = "%Y-%m-%d"),
         recent_sales_email_opened_date = as.Date(recent_sales_email_opened_date, format = "%Y-%m-%d"),
         recent_sales_email_replied_date = as.Date(recent_sales_email_replied_date, format = "%Y-%m-%d"))

