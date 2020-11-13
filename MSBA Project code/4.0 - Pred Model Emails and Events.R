#set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\fresh_data')

## Libraries used

library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)

## Variables used

# current fiscal year
current_fy = ifelse( month(Sys.Date()) > 6, year(Sys.Date()) + 1, 
                     year(Sys.Date())) 


# 2 Data Cleaned Files
# Emails - Computing Averages -----------------------------------------------------------------


indy <- 
  read_csv('individual_info_cleaned.csv') %>%
  select(ID_DEMO, MEMBERSHIP_STATUS_CODE)


emails <- fread('emails_cleaned.csv')


# Only emails since 2014
# Only important categories
# "1" if you opened if the email OR clicked the link after you opened
emails <- emails %>% 
  filter(YEAR_FISCAL > 2014) %>%
  merge.data.table(indy, by = 'ID_DEMO') %>%
  select(ID_DEMO, DATE_CONTACT, CODE_PATH, CODE_OUTCOME, broad_cat) %>%
  filter(broad_cat %in% c('Learning', 'Legislature', 'Social', 'Sports')) %>%
  mutate(ctr = ifelse(CODE_OUTCOME == 'CL', 1, 0)) %>%
  mutate(click_open = ifelse(CODE_OUTCOME == 'CL' | CODE_OUTCOME == 'OE', 1, 0)) 


# Each person's avg click_open rate per category
clickthru_rates <- 
  emails %>% group_by(ID_DEMO, broad_cat) %>%
  dplyr::summarize(total_possible = n(),
                   total_clicked = sum(click_open)) %>% 
  mutate(click_or_open = total_clicked / total_possible)


# Doing a "transpose" type of operation
# Make each category a column, only 1 row per person
clickthru_rates_spread <- 
  reshape2::dcast(data = clickthru_rates,
                  ID_DEMO ~ broad_cat,             
                  value.var = 'click_or_open',    
                  fun=sum) %>%
  rename_at(vars(-ID_DEMO), ~ paste0(., '_emails')) %>% replace(is.na(.), 0) 



# Computing each person's avg ctr across all categories
clickthru_rates_general <- 
  emails %>% group_by(ID_DEMO) %>%
  dplyr::summarize(total_possible = n(),
                   total_clicked = sum(ctr)) %>% 
  mutate(general_ctr = total_clicked / total_possible) %>%
  select(ID_DEMO, general_ctr)



# Combine per-category click_open rates with a general ctr
clickthru_rates_spread <- 
  clickthru_rates_spread %>% 
  inner_join(clickthru_rates_general, by = 'ID_DEMO')



# Events ------------------------------------------------------------------

events <- read_csv('events_cleaned.csv') %>% 
  filter(YEAR_FISCAL > 2014) %>%
  select(ID_DEMO, DATE_EVENT, YEAR_FISCAL, broad_cat, NBR_GROUP) %>% 
  inner_join(indy, by = 'ID_DEMO') 




# Adding a column to show how many of each category a person went to
events_adj <- 
  events %>% 
  count(ID_DEMO, broad_cat) %>% 
  rename(total_type_person = n)



# Transpose operation - getting each category as a column
events_spread <- 
  reshape2::dcast(data = events_adj,
                  ID_DEMO ~ broad_cat,             
                  value.var = 'total_type_person',    
                  fun=sum) %>%
  rename_at(vars(-ID_DEMO), ~ paste0(., '_events')) %>%  
  replace(is.na(.), 0) 



# Counting total events
events_spread <- 
  events_spread %>% 
  mutate(all_events = rowSums(.[2:7]))
  



# Per-person --------------------------------------------------------------

# Connecting emails & events
per_person_with_id <- 
  clickthru_rates_spread %>% 
  full_join(events_spread, by = c('ID_DEMO')) %>% 
  ungroup() %>% 
  inner_join(indy, by = 'ID_DEMO') %>% 
  select(-MEMBERSHIP_STATUS_CODE) %>%
  replace(is.na(.), 0)



fwrite(per_person_with_id, file='./ML files/pred_emails_events.csv')

