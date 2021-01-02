#set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\main_data')

## Libraries used

library(lubridate)
library(data.table)
library(stringr)


# current fiscal year
current_fy = 2021
previous_fy = current_fy - 1


# 2 Data Cleaned Files

indy_info_main <- fread('individual_info_cleaned.csv')
membership_main <- fread('membership_cleaned.csv')

na_members <- copy(indy_info_main)[is.na(MEMBERSHIP_STATUS_CODE)]
past_members <- copy(indy_info_main)[MEMBERSHIP_STATUS_CODE=='P']

non_members <- rbind(na_members, past_members)[,.(ID_DEMO)]

# setwd("D:/Group Folder/Data")

emails_events <- fread('ML files/all_emails_events.csv')




emails_events_member <- merge.data.table(non_members, emails_events, by = c('ID_DEMO'), all.x=TRUE)
emails_events_member[is.na(emails_events_member)] = 0


# Engagement --------------------------------------------------------------

# Load the cleaned engagement file

eng_cleaned <- fread("engagement_cleaned.csv", header = TRUE)


# Average UMN Annual engagement scores for last 5 years for each Alumni - training
last_5_avg_eng <- eng_cleaned[(ID_DEMO %in% non_members$ID_DEMO) & (YEAR_FISCAL >= previous_fy - 5)]

last_5_avg_eng <- last_5_avg_eng[, c(1,3, 4 ,5, 6,7,8, 9)]
last_5_avg_eng <- last_5_avg_eng[, .(UMN_event = mean(EVENT_ATTEND_ANNUAL), 
                                     UMN_member = mean(UMAA_MEMBER_ANNUAL), 
                                     UMN_donor = mean(DONOR_ANNUAL), 
                                     UMN_volun = mean(VOLUNTEER_ANNUAL), 
                                     UMN_inform = mean(STAY_INFORMED_ANNUAL), 
                                     UMN_loyalty = mean(LOYALTY_ANNUAL),
                                     UMN_avg_Annual_score_5_years = mean(ENGAGEMENT_TOTAL_ANNUAL)),
                                 by= .(ID_DEMO)]


# join engagemnet scores with emails_events_membership dataframe - training
all_df <- merge.data.table(last_5_avg_eng, emails_events_member, by = "ID_DEMO")



fwrite(all_df, "Ml files/new_member_all_data.csv", row.names = FALSE)


