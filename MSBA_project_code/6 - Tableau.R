setwd('D:/Group Folder/SEQUENTIAL FILES')

library(data.table)
library(tidyverse)
library(dtplyr)
library(stringr)
library(lubridate)
library(readxl)


# 2 Data Cleaned Files
# Reading in original files - cluster, indy, member --------------------------------------------------------

current_fy <- ifelse(month(Sys.Date()) > 6, year(Sys.Date()) + 1, year(Sys.Date()))

clusters <- read_csv('3 Data Generated Files/per_person_clusters.csv') %>%
  select(ID_DEMO, cluster = Description)



indy <- read_csv('2 Data Cleaned Files/individual_info_cleaned.csv') %>%
  inner_join(clusters, by = 'ID_DEMO') %>%
  mutate(age = AGE) %>%
  filter(MEMBERSHIP_TYPE_CODE != 'X')


# We only care about current status
# Annual, lifetime, non-member
indy <-
  indy %>% 
  mutate(MEMBERSHIP_TYPE_CODE = 
           ifelse(MEMBERSHIP_TYPE_CODE == 'L' & MEMBERSHIP_STATUS_CODE == 'C', 'L',
           ifelse(MEMBERSHIP_TYPE_CODE == 'A' & MEMBERSHIP_STATUS_CODE == 'C', 'A', NA)))


# These next two chunks are just splitting people
# into older vs newer to add a bit more detail
avg_age <- 
  indy %>% 
  filter(!is.na(age)) %>% 
  dplyr::summarize(ages = mean(age))


indy <- indy %>% 
  select(ID_DEMO, age) %>% 
  mutate(age_cuts = ifelse(age > avg_age$ages, 'Older', 'Younger')) %>%   
  select(ID_DEMO, age_cuts)



membership_eda <- read_csv('4 Tableau/membership_EDA.csv') %>%
  inner_join(clusters, by = 'ID_DEMO') %>% 
  select(ID_DEMO, YEAR_FISCAL = FISCAL_YEAR, member = member_status) %>%
  filter(YEAR_FISCAL > 2014)



membership_clean <- membership_eda



# How many people had "x" characteristics at the time?
# This is showing us the opportunity.
# We will be combining this with our other data
# frames later on to compare opportunity vs results
mem_ind_clu <- 
  membership_clean %>% 
  inner_join(clusters, by = 'ID_DEMO') %>%
  left_join(indy, by = 'ID_DEMO') %>%
  count(year_tableau = YEAR_FISCAL, member, age_cuts, cluster) %>% 
  rename(total_possible_people = n) %>%
  filter(!is.na(cluster))



# Engagement --------------------------------------------------------------

# This whole section is creating the "engagement" piece of the dashboard.


engage <- read_csv('2 Data Cleaned Files/engagement_cleaned.csv') %>%
  filter(YEAR_FISCAL > 2000) %>%
  inner_join(clusters, by = 'ID_DEMO') %>%
  select(ID_DEMO, YEAR_FISCAL, DONOR_ANNUAL, VOLUNTEER_ANNUAL, 
         STAY_INFORMED_ANNUAL, LOYALTY_ANNUAL, EVENT_ATTEND_ANNUAL, UMAA_MEMBER_ANNUAL) 


# Engagement originally had separate columns for each type.
# We need to bring each engage type into 1 column to compute summaries
engage_gather <- engage %>% 
  gather(key = engage_type,
         value = given_value,
         DONOR_ANNUAL:UMAA_MEMBER_ANNUAL)


# Adding persons chars into their engagement - helps show detail
engage_join <- 
  engage_gather %>% 
  inner_join(clusters, by = 'ID_DEMO') %>%
  inner_join(membership_clean, by = c('ID_DEMO', 'YEAR_FISCAL')) %>%
  inner_join(indy, by = 'ID_DEMO')


# Summary - how did the cluster perform? (The results)
engage_for_tableau <- 
  engage_join %>% 
  group_by(year_tableau = YEAR_FISCAL,
           cluster, 
           age_cuts, 
           engage_type,
           member) %>%
  dplyr::summarize(engage_values = sum(given_value)) %>% ungroup()



# How many people were there in the group? (The opportunity & the results)
engage_for_tableau <- 
  engage_for_tableau %>% 
  left_join(mem_ind_clu, by = c('year_tableau', 'member', 'age_cuts', 'cluster'))


# Events -----------------------------------------------------------------

# Makes sure we don't have bad data / future events in our system
# max_event_date <- 
#   read_csv('D:/Group Folder/Data/Cleaned data sets/events_cleaned.csv') %>%
#   filter(DATE_EVENT <= Sys.Date()) 
max_event_date <- 
  read_csv('2 Data Cleaned Files/events_cleaned.csv') %>%
  filter(DATE_EVENT <= Sys.Date()) 



# Making sure data is correct, adding in clusters 
events <- read_csv('2 Data Cleaned Files/events_cleaned.csv') %>% 
  inner_join(clusters, by = 'ID_DEMO') %>%
  mutate(DATE_EVENT = ymd(DATE_EVENT)) %>%
  filter(DATE_EVENT <= Sys.Date()) %>%
  filter(YEAR_FISCAL > 2014) %>%
  select(ID_DEMO, YEAR_FISCAL, broad_cat) 


# Adding person's characteristics
events <- 
  events %>% 
  inner_join(membership_clean, by = c('ID_DEMO', 'YEAR_FISCAL')) %>%
  inner_join(indy, by = 'ID_DEMO') %>%
  inner_join(clusters, by = 'ID_DEMO')



# Number of events per group
events_for_tableau <- events %>% 
  count(year_tableau = YEAR_FISCAL,
        cluster, 
        age_cuts, 
        Event_Category = broad_cat,
        member) %>% 
  rename(event_values = n)



# How many people in the group? (Opportunity)
events_for_tableau <- 
  events_for_tableau %>% 
  inner_join(mem_ind_clu, 
            by = c('year_tableau', 'member', 'age_cuts', 'cluster'))





# Membership --------------------------------------------------------------

# Add persons characteristics
membership_join <- 
  membership_clean %>% 
  inner_join(indy, by = 'ID_DEMO') %>%
  inner_join(clusters, by = 'ID_DEMO')



# Summary per group - what is the membership makeup of the cluster?
membership_tab <- 
  membership_join %>% 
  count(year_tableau = YEAR_FISCAL,
        cluster, 
        age_cuts, 
        member) %>% 
  rename(member_values = n)


# Summary per group (Results)
ind_clu <- 
  membership_join %>% 
  count(year_tableau = YEAR_FISCAL,
        cluster, 
        age_cuts) %>% 
  rename(total_possible_people = n)


# How many people in the group? (Results and opportunity)
membership_tab <- 
  membership_tab %>% 
  inner_join(ind_clu, by = c('cluster', 'year_tableau', 'age_cuts'))


# Emails -----------------------------------------------------------------


emails2 <- fread('./2 Data Cleaned Files/emails_cleaned.csv', 
                 select = c('broad_cat', 'ID_DEMO', 'DATE_CONTACT', 
                            'CODE_OUTCOME', 'YEAR_FISCAL', 'Status_Clicked')) 


# Adding in all the details
emails <- emails2 %>%
  merge.data.table(clusters, by = 'ID_DEMO') %>%
  merge.data.table(membership_clean, by = c('ID_DEMO', 'YEAR_FISCAL')) %>%
  merge.data.table(indy, by = 'ID_DEMO') %>% 
  mutate(DATE_CONTACT = ymd(DATE_CONTACT)) %>%
  select(broad_cat, ID_DEMO, YEAR_FISCAL, CODE_OUTCOME, age_cuts, cluster, member, Status_Clicked)



# CTR per cluster and per category
emails_groups <- 
  emails %>%
  group_by(year_tableau = YEAR_FISCAL, 
           cluster, 
           age_cuts,
           Email_Category = broad_cat,
           member) %>%
 dplyr::summarize(Total_Emails_Sent = n(),
            Total_Emails_Clicked = sum(Status_Clicked)) %>% ungroup()



# Formatting --------------------------------------------------------------

# This is just getting everything ready to go into Tableau.
# We are adding in "Cluster" before the actual cluster number so Tableau doesn't get confused.
# We are adding in the month/day to signal the beginning of the FY to Tableau

engage_for_tableau <- 
  engage_for_tableau %>% 
  mutate(Month = 7, Day = 1, 
         year_tableau = make_date(year_tableau, Month, Day)) %>%
  select(year_tableau, cluster, age_cuts, member, engage_type, engage_values, total_possible_people)


emails_groups <- 
  emails_groups %>% 
  mutate(Month = 7, Day = 1, 
         year_tableau = make_date(year_tableau, Month, Day)) %>%
  select(year_tableau, cluster, age_cuts, member, Email_Category, Total_Emails_Sent, Total_Emails_Clicked)


membership_tab <- 
  membership_tab %>% 
  mutate(Month = 7, Day = 1, 
         year_tableau = make_date(year_tableau, Month, Day)) %>%
  select(year_tableau, cluster, age_cuts, member, member_values, total_possible_people)


events_for_tableau <- 
  events_for_tableau %>% 
  mutate(Month = 7, Day = 1, 
         year_tableau = make_date(year_tableau, Month, Day)) %>%
  select(year_tableau, cluster, age_cuts, member, Event_Category, event_values, total_possible_people)


write.csv(emails_groups, '4 Tableau/year_emails_for_tableau.csv')
write.csv(membership_tab, '4 Tableau/membership_for_tableau.csv')
write.csv(engage_for_tableau, '4 Tableau/engage_for_tableau.csv')
write.csv(events_for_tableau, '4 Tableau/year_events_for_tableau.csv')


