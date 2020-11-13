#set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\fresh_data')

## Libraries used

library(tidyverse)
library(lubridate)
library(data.table)
library(dplyr)

## variables used

# current fiscal year
current_fy = ifelse( month(Sys.Date()) > 6, year(Sys.Date()) + 1, 
                     year(Sys.Date())) 
previous_fy = current_fy - 1


# 2 Data Cleaned Files
# Membership ---------------------------------------------------------------------

# setwd("D:/Group Folder/Data/Cleaned data sets")
# indy_info <- read_csv('Individual_Info_Cleaned.csv')
# setwd("D:/Group Folder/Data")
# membership_old <- read_csv('membership_clean.csv')


indy_info <- read_csv('individual_info_cleaned.csv')
membership_old <- read_csv('membership_cleaned.csv')



### A - Initial Member engineering to expand data across years 

# Create membership df to capture last 5 year window
membership = membership_old %>%
  filter((FISCAL_YEAR > current_fy - 6 & FISCAL_YEAR < current_fy)) %>%
  select(ID_DEMO, APPEAL_CODE, AMOUNT_PAID, FISCAL_YEAR, MEMBERSHIP_LEVEL, ID_OF_RECORD_WITH_RELATIONSHIP) %>%
  mutate(life_before_window = 0)

# Spreading per person
per_indy <- 
  reshape2::dcast(data = membership,
                  ID_DEMO ~ FISCAL_YEAR,   
                  value.var = 'AMOUNT_PAID', 
                  fun=sum)

# Gathering back
new_membership <- 
  per_indy %>% 
  gather(key = "FISCAL_YEAR",
         value = 'AMOUNT_PAID',
         -ID_DEMO) %>%
  mutate(FISCAL_YEAR = as.numeric(FISCAL_YEAR))

# Person + year combo``
membership_join <- new_membership %>% left_join(membership, by = c('ID_DEMO', 'FISCAL_YEAR', 'AMOUNT_PAID'))

# Identifying all true comp members - must have a. "comp" in membership title b. $0 total spent (used in rollup section)
comp_alumni = membership_old %>% filter(grepl("Comp",MEMBERSHIP_LEVEL)) %>% select(ID_DEMO) %>% distinct()
zero_spend_alumni = membership_old %>% group_by(ID_DEMO) %>% summarise(total_spend = sum(AMOUNT_PAID)) %>% filter(total_spend == 0)
comp_alumni_list = inner_join(comp_alumni, zero_spend_alumni, by = "ID_DEMO") %>% select(-total_spend) %>% mutate(comp_flag = 1)

### B - Merging Individual Info to Include non-members 

# Find ALL current lifetime members that purchased memberships before 2015
ind_info_lt = indy_info %>% filter(MEMBERSHIP_TYPE_CODE == "L") %>% select(ID_DEMO) %>% filter(!(ID_DEMO %in% membership$ID_DEMO)) 

# Place them in a DF
records_minus5 = ind_info_lt %>% mutate(FISCAL_YEAR = current_fy-5)
records_minus4 = ind_info_lt %>% mutate(FISCAL_YEAR = current_fy-4)
records_minus3 = ind_info_lt %>% mutate(FISCAL_YEAR = current_fy-3)
records_minus2 = ind_info_lt %>% mutate(FISCAL_YEAR = current_fy-2)
records_LastYear = ind_info_lt %>% mutate(FISCAL_YEAR = current_fy-1)

records_new_life_members = rbind(records_minus5,records_minus4,records_minus3,records_minus2,records_LastYear) %>% 
                           mutate(APPEAL_CODE = NA, 
                                  AMOUNT_PAID = NA,
                                  MEMBERSHIP_LEVEL = NA, 
                                  ID_OF_RECORD_WITH_RELATIONSHIP = NA,
                                  life_before_window = 1)

# Get all records in individual_info that have never been members
indy_info_join <- indy_info %>% filter(!(ID_DEMO %in% membership$ID_DEMO) & 
                                       !(ID_DEMO %in% records_new_life_members$ID_DEMO)) %>% 
                                select(ID_DEMO) 

# Place them in a DF
records_minus5 = indy_info_join %>% mutate(FISCAL_YEAR = current_fy-5)
records_minus4 = indy_info_join %>% mutate(FISCAL_YEAR = current_fy-4)
records_minus3 = indy_info_join %>% mutate(FISCAL_YEAR = current_fy-3)
records_minus2 = indy_info_join %>% mutate(FISCAL_YEAR = current_fy-2)
records_LastYear = indy_info_join %>% mutate(FISCAL_YEAR = current_fy-1)

records_new_non_members = rbind(records_minus5,records_minus4,records_minus3,records_minus2,records_LastYear) %>% 
                          mutate(APPEAL_CODE = NA, 
                                 AMOUNT_PAID = NA,
                                 MEMBERSHIP_LEVEL = NA, 
                                 ID_OF_RECORD_WITH_RELATIONSHIP = NA,
                                 life_before_window = NA)


# Combine 1. All members with transactions since first year of window (membership_join), 
# 2. All life-members before window (records_new_life_members), 3. All others (records_new_non_members)

membership_join = rbind(membership_join, records_new_life_members, records_new_non_members)




### C - Feature Engineering

# Annual or lifetime or non-member (accounting for multi-year memberships and lifetime purchases in later code chunks)
membership_join <- membership_join %>% 
  mutate(member_status = ifelse(is.na(MEMBERSHIP_LEVEL), 'non-member', 
                                ifelse(str_detect(MEMBERSHIP_LEVEL, 'Life'), 'life-member', 'annual-member')))

# Joint memberships
membership_join <- membership_join %>% mutate(joint = ifelse(is.na(ID_OF_RECORD_WITH_RELATIONSHIP), 0, 1))

# How many years of Annual? (Not a 1:1 mapping as person may have purchased multi-year - Only used for rollup and spread code)
regexp <- "[[:digit:]]+"

# First we assign ann_num_years the numeric value we have extracted from the MEMBERSHIP_LEVEL 
#  column if and only if there is a numeric value present
membership_join <- membership_join %>% mutate(ann_num_years = str_extract(MEMBERSHIP_LEVEL, regexp))

# Next we use the ann_num_years column to determine the numeric value to assign.
# a: If ann_num_years is NA (meaning no digit was found in MEMBERHSIP_STATUS) and the word "Annual" 
#    was found in MEMEBRSHIP_STATUS then we classify the purchase as a one year annual membership
# b: If "Life" is found in membership status then we classify as 0 annual years
# c: If the word "Annual" was found and ann_num_years is not NA then we know there was a 
#    multi-year purchase made and we assign that value to ann_num_years
membership_join <- 
  membership_join %>% mutate(ann_num_years = ifelse(is.na(ann_num_years) & grepl("Annual",MEMBERSHIP_LEVEL), 1,
                                                    ifelse(grepl("Life",MEMBERSHIP_LEVEL) | is.na(MEMBERSHIP_LEVEL), 0, ann_num_years)))
membership_join$ann_num_years = as.numeric(membership_join$ann_num_years)




### D - Accounting for Multi-year purchase types in member_status

## Annual:
# Add new columns indicating members' 2,3,4,5 year purchases carying forward
membership_join = membership_join %>% group_by(ID_DEMO) %>% arrange(FISCAL_YEAR) %>% 
  mutate(prev1 = lag(ann_num_years)) %>%
  mutate(prev2 = lag(ann_num_years,2)) %>%
  mutate(prev3 = lag(ann_num_years,3)) %>%
  mutate(prev4 = lag(ann_num_years,4))

# Replace NA's with zero for mutation step to work
membership_join$prev1 = membership_join$prev1 %>% replace_na(0)
membership_join$prev2 = membership_join$prev2 %>% replace_na(0)
membership_join$prev3 = membership_join$prev3 %>% replace_na(0)
membership_join$prev4 = membership_join$prev4 %>% replace_na(0)

# Fill the member_status column based on multi-year purchases
membership_join = membership_join %>% mutate(member_status = ifelse((prev1 == 2 | prev1 == 3 | prev1 == 4 | prev1 == 5), "annual-member", member_status))
membership_join = membership_join %>% mutate(member_status = ifelse((prev2 == 3 | prev2 == 4 | prev2 == 5), "annual-member", member_status ))
membership_join = membership_join %>% mutate(member_status = ifelse((prev3 == 4 | prev3 == 5), "annual-member", member_status ))
membership_join = membership_join %>% mutate(member_status = ifelse((prev4 == 5), "annual-member", member_status ))

# Remove columns used in this engineering
membership_join = membership_join %>% select(-prev1, -prev2, -prev3, -prev4)


## Life:
# Fill Lifetime members' member_status with "life-member" for years after they made their lifetime purchase
membership_join = membership_join %>% group_by(ID_DEMO) %>% arrange(FISCAL_YEAR) %>% 
  mutate(prev1 = lag(member_status)) %>%
  mutate(prev2 = lag(member_status,2)) %>%
  mutate(prev3 = lag(member_status,3)) %>%
  mutate(prev4 = lag(member_status,4))

# Replace NA's with zero for mutation step to work
membership_join$prev1 = membership_join$prev1 %>% replace_na(0)
membership_join$prev2 = membership_join$prev2 %>% replace_na(0)
membership_join$prev3 = membership_join$prev3 %>% replace_na(0)
membership_join$prev4 = membership_join$prev4 %>% replace_na(0)

# Fill the member_status column based on lifetime purchases
membership_join = membership_join %>% 
  mutate(member_status = ifelse((prev1 == "life-member" | 
                                   prev2 == "life-member" | 
                                   prev3 == "life-member" | 
                                   prev4 == "life-member"), "life-member", member_status))

# Remove columns used in this engineering
membership_join = membership_join %>% select(-prev1, -prev2, -prev3, -prev4)




### E - Rollup Prep

# Remove duplicates
membership_join = membership_join %>% distinct(ID_DEMO, FISCAL_YEAR, .keep_all = TRUE)

# Life members before window
membership_join$life_before_window = membership_join$life_before_window %>% replace_na(0)
membership_join = membership_join %>% mutate(member_status = ifelse(life_before_window == 1, "life-member", member_status))

# Add a life_year column for rollup
membership_join <- membership_join %>% mutate(life_yr = ifelse(grepl("Life",MEMBERSHIP_LEVEL), FISCAL_YEAR, 0))

# Handle installments
membership_join = membership_join %>% 
  mutate(ann_num_years = ifelse(grepl("Install",MEMBERSHIP_LEVEL), 0, ann_num_years))


### F - Rollup

TARGET_YR_GLOBAL = current_fy - 1
TRAIN_DATA_YRS = 4

# Get the target values
membership_LastYear = membership_join %>% filter(FISCAL_YEAR == current_fy - 1) %>% 
  rename(membership_LastYear = member_status) %>% 
  select(ID_DEMO, membership_LastYear)

membership_TwoYearsAgo = membership_join %>% filter(FISCAL_YEAR == current_fy - 2) %>% 
  rename(membership_TwoYearsAgo = member_status) %>% 
  select(ID_DEMO, membership_TwoYearsAgo)

# Remove membership_LastYear to not include it in the rollup counts
membership_join_rollup = membership_join %>% filter(FISCAL_YEAR != current_fy - 1)

# Add non-mem years
membership_join_rollup = membership_join_rollup %>% mutate(nonmem_num_years = ifelse(member_status == "non-member", 1, 0))

# aggregate 
membership_rollup = membership_join_rollup %>% group_by(ID_DEMO) %>% 
  summarise(annual_years = sum(as.numeric(ann_num_years)),
            life_years = TARGET_YR_GLOBAL - sum(life_yr),
            nonmem_years = sum(nonmem_num_years),
            life_before_window_flag = sum(life_before_window))

membership_rollup = membership_rollup %>% mutate(annual_years = ifelse(annual_years > TRAIN_DATA_YRS, TRAIN_DATA_YRS, annual_years),
                                                 life_years = ifelse(life_years == TARGET_YR_GLOBAL, 0, life_years),
                                                 life_years = ifelse(life_before_window_flag == TRAIN_DATA_YRS, 4,life_years))

# Join the LastYear and TwoYearsAgo member_status columns
membership_rollup = membership_rollup %>% left_join(membership_LastYear, by = "ID_DEMO") 
membership_rollup = membership_rollup %>% left_join(membership_TwoYearsAgo, by = "ID_DEMO") 

# Confirm anyone with life years is classified as a life member in target variable
membership_rollup = membership_rollup %>% mutate(membership_LastYear = ifelse(life_years == 0, membership_LastYear, "life-member"))

# Adjust rows that are not calculating life_years correctly (due to installments)
membership_rollup = membership_rollup %>% 
  mutate(life_years = ifelse(life_years < 0 | life_years > 4, 
                             (TRAIN_DATA_YRS - nonmem_years - annual_years),
                             life_years))

# Add flag for comp members 
membership_rollup = membership_rollup %>% left_join(comp_alumni_list, by = "ID_DEMO")
membership_rollup$comp_flag = membership_rollup$comp_flag %>% replace_na(0)


# Remove columns used in this engineering
membership_rollup = membership_rollup %>% select(-life_before_window_flag)

# Create separate data frame for prediction. This now ill include the most 
#   recent year in rollups based on their most recent status
membership_rollup_prediction = membership_rollup %>% 
  mutate(annual_years = ifelse(membership_LastYear == "annual-member", 
                               annual_years + 1 , 
                               annual_years)) %>%
  mutate(life_years = ifelse(membership_LastYear == "life-member", 
                             life_years + 1 , 
                             life_years)) %>%
  mutate(nonmem_years = ifelse(membership_LastYear == "non-member", 
                               nonmem_years + 1 , 
                               nonmem_years))

# write to this DF that will be used in join operations below
member = membership_rollup
member_prediction = membership_rollup_prediction



# Emails & Events --------------------------------------------------------------
member_filter <- member %>% select(ID_DEMO)
member_prediction_filter = member_prediction %>% select(ID_DEMO)

# setwd("D:/Group Folder/Data")
# emails_events <- read_csv('pred_emails_events.csv')
emails_events <- read_csv('3 Data Generated Files/pred_emails_events.csv')



emails_events_member <- 
                    member %>%
                    left_join(emails_events, by = c('ID_DEMO')) %>%
                    replace(is.na(.), 0) %>%
                    ungroup()

emails_events_member_prediction <- 
                    member_prediction %>%
                    left_join(emails_events, by = c('ID_DEMO')) %>%
                    replace(is.na(.), 0) %>%
                    ungroup()


# Engagement --------------------------------------------------------------

# Load the cleaned engagement file
# eng_cleaned <- read.csv("D:/Group Folder/Data/Cleaned data sets/engagement_cleaned.csv",
#                         header = TRUE)
eng_cleaned <- read.csv("2 Data Cleaned Files/engagement_cleaned.csv", header = TRUE)


# Average UMN Annual engagement scores for last 5 years for each Alumni - training
UMN_last_5_avg_training <- eng_cleaned %>%
                          filter( (YEAR_FISCAL >= previous_fy - 5) & (YEAR_FISCAL != previous_fy) ) %>%
                          select(c(1,3, 4 ,5, 6,7,8, 9)) %>%
                          group_by(ID_DEMO) %>%
                          summarise(UMN_event = mean(EVENT_ATTEND_ANNUAL), 
                                    UMN_member = mean(UMAA_MEMBER_ANNUAL), 
                                    UMN_donor = mean(DONOR_ANNUAL), 
                                    UMN_volun = mean(VOLUNTEER_ANNUAL), 
                                    UMN_inform = mean(STAY_INFORMED_ANNUAL), 
                                    UMN_loyalty = mean(LOYALTY_ANNUAL),
                                    UMN_avg_Annual_score_5_years = mean(ENGAGEMENT_TOTAL_ANNUAL))

# Average UMN Annual engagement scores for last 5 years for each Alumni - future prediction
UMN_last_5_avg_future_pred <- eng_cleaned %>%
                            filter( (YEAR_FISCAL >= current_fy - 5) & (YEAR_FISCAL != current_fy) ) %>%
                            select(c(1,3, 4 ,5, 6,7,8, 9)) %>%
                            group_by(ID_DEMO) %>%
                            summarise(UMN_event = mean(EVENT_ATTEND_ANNUAL), 
                                      UMN_member = mean(UMAA_MEMBER_ANNUAL), 
                                      UMN_donor = mean(DONOR_ANNUAL), 
                                      UMN_volun = mean(VOLUNTEER_ANNUAL), 
                                      UMN_inform = mean(STAY_INFORMED_ANNUAL), 
                                      UMN_loyalty = mean(LOYALTY_ANNUAL),
                                      UMN_avg_Annual_score_5_years = mean(ENGAGEMENT_TOTAL_ANNUAL))


# join engagemnet scores with emails_events_membership dataframe - training
training_df <- UMN_last_5_avg_training %>%
               inner_join(emails_events_member, by = "ID_DEMO")

# join engagemnet scores with emails_events_membership dataframe - future prediction
future_prediction_df <- UMN_last_5_avg_future_pred %>%
                        inner_join(emails_events_member_prediction, by = "ID_DEMO")



# setwd("D:/Group Folder/Data/Cleaned data sets")
# write.csv(training_df, "Data_new_member_training_final.csv", row.names = FALSE)
# write.csv(future_prediction_df, "Data_new_member_future_prediction_final.csv", row.names = FALSE)

write.csv(training_df, "3 Data Generated Files/Data_new_member_training_final.csv", row.names = FALSE)
write.csv(future_prediction_df, "3 Data Generated Files/Data_new_member_future_prediction_final.csv", row.names = FALSE)
