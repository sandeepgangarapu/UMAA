## Libraries used

library(tidyverse)
library(flexclust)
library(data.table)
library(dtplyr)
library(lubridate)

## Variables used


# If its July or later, the fiscal year = our current year
current_fy = ifelse(month(Sys.Date()) > 6, year(Sys.Date()) + 1, 
                     year(Sys.Date())) 

## set working directory
setwd("D:/Group Folder/SEQUENTIAL FILES")



# Emails - Computing Averages -----------------------------------------------------------------


indy <- 
  read_csv('2 Data Cleaned Files/individual_info_cleaned.csv') %>%
  select(ID_DEMO, MEMBERSHIP_TYPE_CODE, MEMBERSHIP_STATUS_CODE)



emails <- fread('./2 Data Cleaned Files/emails_cleaned.csv')



# "1" if you opened if the email or clicked the link
emails <- emails %>% 
  merge.data.table(indy, by = 'ID_DEMO') %>%
  select(ID_DEMO, DATE_CONTACT, YEAR_FISCAL, CODE_PATH, CODE_OUTCOME, broad_cat) %>%
  filter(broad_cat %in% c('Learning', 'Legislature', 'Social', 'Sports')) %>%
  mutate(click_open = ifelse(CODE_OUTCOME == 'CL' | CODE_OUTCOME == 'OE', 1, 0)) 



# Each person's avg click_open rate
email_avg <- emails %>% group_by(ID_DEMO) %>%
  dplyr::summarize(total_possible = n(),
                   total_clicked = sum(click_open)) %>% 
  mutate(avg_click_or_open = total_clicked / total_possible) %>%
  select(ID_DEMO, avg_click_or_open)



# Each person's avg click_open rate per category
clickthru_rates <- 
  emails %>% group_by(ID_DEMO, broad_cat) %>%
  dplyr::summarize(total_possible = n(),
                   total_clicked = sum(click_open)) %>% 
  mutate(click_or_open = total_clicked / total_possible) %>%
  select(ID_DEMO, broad_cat, click_or_open)




# Spreading ---------------------------------------------------------------

# Make each category a column
clickthru_rates_spread <- 
  clickthru_rates %>% spread(broad_cat, click_or_open) %>%
  rename(email_Learning = Learning,
         email_Legislature = Legislature,
         email_Social = Social,
         email_Sports = Sports) %>% replace(is.na(.), 0) 


# Find the "adjusted" open rate per category
clickthru_rates_spread <- 
  clickthru_rates_spread %>% 
  inner_join(email_avg, by = 'ID_DEMO') %>%
  mutate(email_Learning = email_Learning - avg_click_or_open, 
         email_Legislature = email_Legislature - avg_click_or_open, 
         email_Social = email_Social - avg_click_or_open, 
         email_Sports = email_Sports - avg_click_or_open) %>% 
  select(-avg_click_or_open)


# Events ------------------------------------------------------------------


events <- read_csv('2 Data Cleaned Files/events_cleaned.csv') %>% 
  select(ID_DEMO, DATE_EVENT, YEAR_FISCAL, broad_cat, NBR_GROUP) %>% 
  inner_join(indy, by = 'ID_DEMO') 


# How many event types are there?
# How many has the person gone to?
# Their total events attended / total possible types
poss_cats <- n_distinct(events$broad_cat)
event_person_avg <- 
  events %>% 
  group_by(ID_DEMO) %>% 
  summarise(counts = n(), 
            expected_per_cat = counts / poss_cats) %>%
  select(ID_DEMO, expected_per_cat)


# Adding a column to show how many of each type a person went to
events_adj <- 
  events %>% 
  count(ID_DEMO, broad_cat) %>% 
  rename(total_type_person = n) %>%
  select(ID_DEMO, broad_cat, total_type_person)



# Getting each cat as a column
events_spread <- 
  events_adj %>%
  spread(broad_cat, total_type_person) %>% 
  rename(event_Learning = Learning,
         event_Legislature = Legislature,
         event_other_networking = Networking,
         event_Social = Social,
         event_Sports = Sports) %>% 
  select(-Other) %>% replace(is.na(.), 0) 


# Getting the "adjusted" counts
events_spread <- 
  events_spread %>% 
  inner_join(event_person_avg, by ='ID_DEMO') %>%
  mutate(event_Learning = event_Learning - expected_per_cat, 
         event_Legis = event_Legislature - expected_per_cat, 
         event_other_networking = event_other_networking  - expected_per_cat, 
         event_Social = event_Social - expected_per_cat,
         event_Sports = event_Sports - expected_per_cat) %>%
  select(-expected_per_cat)

# Per-person --------------------------------------------------------------

# Connecting emails & events
per_person_with_id <- 
  clickthru_rates_spread %>% 
  full_join(events_spread, by = 'ID_DEMO') %>% 
  ungroup() %>% 
  inner_join(indy, by = 'ID_DEMO')



# Creating improved columns for our clustering
cluster_df <- 
  per_person_with_id %>%
  replace(is.na(.), 0) %>% as_tibble() %>% 
  mutate(learning = event_Learning + email_Learning,
         legis = event_Legis + email_Legislature,
         social = email_Social + event_Social,
         sports = email_Sports + event_Sports,
         networking = event_other_networking + event_other_networking) %>%
  select(ID_DEMO, learning, legis, social, sports, networking, MEMBERSHIP_TYPE_CODE, MEMBERSHIP_STATUS_CODE)


write.csv(cluster_df, '3 Data Generated Files/cluster_prep.csv', row.names = F)

