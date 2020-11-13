#set working directory
setwd('D:/Group Folder/SEQUENTIAL FILES')


## Libraries used
library(tidyverse)
library(lubridate)

## variables used

# current fiscal year
current_fy = ifelse( month(Sys.Date()) > 6, year(Sys.Date()) + 1, 
                     year(Sys.Date())) 




# Pat - Membership ---------------------------------------------------------------------

# setwd("D:/Group Folder/Data")


membership <- read_csv('2 Data Cleaned Files/membership_cleaned.csv')
membership_old <- membership

membership = membership_old %>%
  filter((FISCAL_YEAR > 2014 & FISCAL_YEAR < 2021)) %>%
  # filter(ID_DEMO == "000DE8085A892987D25B02489E3D4667" |
  # ID_DEMO == "432B5C8FFCFAB9E192891482A4A3E699" |
  # ID_DEMO == "2175EC9FA7240CFA938E1B98B7588AA4" |
  # ID_DEMO == "B0714092F3B37F6EAD1FB29CBD5CC64F") %>%
  select(ID_DEMO, APPEAL_CODE, AMOUNT_PAID, FISCAL_YEAR, MEMBERSHIP_LEVEL, ID_OF_RECORD_WITH_RELATIONSHIP) %>%
  mutate(life_before_2015 = 0)

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

# Person + year combo
membership_join <- new_membership %>% left_join(membership, by = c('ID_DEMO', 'FISCAL_YEAR', 'AMOUNT_PAID'))

# Merge individual infor to find ALL current lifetime members
ind_info <- read_csv('2 Data Cleaned Files/individual_info_cleaned.csv')
ind_info = ind_info %>% filter(MEMBERSHIP_TYPE_CODE == "L")

# Select only the Lifetime members, not having any rows between 2015-2020
records = ind_info %>% filter(!(ID_DEMO %in% membership$ID_DEMO)) %>% select(ID_DEMO)

# Place them in a DF
records15 = records %>% mutate(FISCAL_YEAR = 2015)
records16 = records %>% mutate(FISCAL_YEAR = 2016)
records17 = records %>% mutate(FISCAL_YEAR = 2017)
records18 = records %>% mutate(FISCAL_YEAR = 2018)
records19 = records %>% mutate(FISCAL_YEAR = 2019)
records20 = records %>% mutate(FISCAL_YEAR = 2020)

records_new = rbind(records15,records16,records17,records18,records19,records20) %>% mutate(APPEAL_CODE = NA, 
                                                                                     AMOUNT_PAID = NA,
                                                                                     MEMBERSHIP_LEVEL = NA, 
                                                                                     ID_OF_RECORD_WITH_RELATIONSHIP = NA,
                                                                                     life_before_2015 = 1)
# Add these members to the data frame
membership_join = rbind(membership_join, records_new)

# Annual or lifetime or non-member (accounting for multi-year memberships and lifetime purchases in later code chunks)
membership_join <- 
  membership_join %>% 
  mutate(member_status = ifelse(is.na(MEMBERSHIP_LEVEL), 'non-member', 
                                ifelse(str_detect(MEMBERSHIP_LEVEL, 'Annual'), 'annual-member', 'life-member')))

# Joint memberships
membership_join <- membership_join %>% mutate(joint = ifelse(is.na(ID_OF_RECORD_WITH_RELATIONSHIP), 0, 1))

# How many years of Annual? (Not a 1:1 mapping as person may have purchased multi-year - Only used for rollup and spread code)
regexp <- "[[:digit:]]+"
membership_join <- membership_join %>% mutate(ann_num_years = str_extract(MEMBERSHIP_LEVEL, regexp))
membership_join <- 
  membership_join %>% mutate(ann_num_years = ifelse(is.na(ann_num_years) & grepl("Annual",MEMBERSHIP_LEVEL), 1,
                                                    ifelse(grepl("Life",MEMBERSHIP_LEVEL) | is.na(MEMBERSHIP_LEVEL), 0, ann_num_years)))
membership_join$ann_num_years = as.numeric(membership_join$ann_num_years)

# Make sure that people who bought 2,3,4,5 year memberships are accounted for in the member_status column
membership_join = membership_join %>% group_by(ID_DEMO) %>% arrange(FISCAL_YEAR) %>% 
  mutate(prev1 = lag(ann_num_years)) %>%
  mutate(prev2 = lag(ann_num_years,2)) %>%
  mutate(prev3 = lag(ann_num_years,3)) %>%
  mutate(prev4 = lag(ann_num_years,4))

membership_join$prev1 = membership_join$prev1 %>% replace_na(0)
membership_join$prev2 = membership_join$prev2 %>% replace_na(0)
membership_join$prev3 = membership_join$prev3 %>% replace_na(0)
membership_join$prev4 = membership_join$prev4 %>% replace_na(0)

membership_join = membership_join %>% mutate(member_status = ifelse((prev1 == 2 | prev1 == 3 | prev1 == 4 | prev1 == 5), "annual-member", member_status))
membership_join = membership_join %>% mutate(member_status = ifelse((prev2 == 3 | prev2 == 4 | prev2 == 5), "annual-member", member_status ))
membership_join = membership_join %>% mutate(member_status = ifelse((prev3 == 4 | prev3 == 5), "annual-member", member_status ))
membership_join = membership_join %>% mutate(member_status = ifelse((prev4 == 5), "annual-member", member_status ))

membership_join = membership_join %>% select(-prev1, -prev2, -prev3, -prev4)

# Fill Lifetime members' member_status with "life-member" for years after they made their lifetime purchase
membership_join = membership_join %>% group_by(ID_DEMO) %>% arrange(FISCAL_YEAR) %>% 
  mutate(prev1 = lag(member_status)) %>%
  mutate(prev2 = lag(member_status,2)) %>%
  mutate(prev3 = lag(member_status,3)) %>%
  mutate(prev4 = lag(member_status,4)) %>%
  mutate(prev5 = lag(member_status,5))

membership_join$prev1 = membership_join$prev1 %>% replace_na(0)
membership_join$prev2 = membership_join$prev2 %>% replace_na(0)
membership_join$prev3 = membership_join$prev3 %>% replace_na(0)
membership_join$prev4 = membership_join$prev4 %>% replace_na(0)
membership_join$prev5 = membership_join$prev5 %>% replace_na(0)

membership_join = membership_join %>% 
  mutate(member_status = ifelse((prev1 == "life-member" | 
                                   prev2 == "life-member" | 
                                   prev3 == "life-member" | 
                                   prev4 == "life-member" |
                                   prev5 == "life-member"), "life-member", member_status))

membership_join = membership_join %>% select(-prev1, -prev2, -prev3, -prev4, -prev5)

# Life members before 2015
membership_join$life_before_2015 = membership_join$life_before_2015 %>% replace_na(0)
membership_join = membership_join %>% mutate(member_status = ifelse(life_before_2015 == 1, "life-member", member_status))

# Add Comp membership Flag
membership_join = membership_join %>% mutate(comp_flag = ifelse(grepl("Comp",MEMBERSHIP_LEVEL), 1, 0))

# Write 
write.csv(membership_join, "4 Tableau/membership_EDA.csv")
