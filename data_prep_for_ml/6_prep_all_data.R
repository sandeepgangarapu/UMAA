#set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\fresh_data')

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

# we only consider last 5 years as active. 
# there are two types of membership. Annual and Life
# Life members will be active every year they purchase the membership
# Annual member will be active for the number of years they purchase the membership after that they become inactive
# Annual members whose memberhsip expires 5 yrs ago will be considered non members



# Group_2 Annual members in the last 5 years
# we consider those who were annual members before 5 yrs as non members and potential people to target

membership_last5 <- membership_main[FISCAL_YEAR>current_fy-6 & FISCAL_YEAR < current_fy, ][, .(ID_DEMO, ID_OF_RECORD_WITH_RELATIONSHIP, MEMBERSHIP_LEVEL, FISCAL_YEAR)]
annual_members <- membership_last5[!MEMBERSHIP_LEVEL %like% "Life",]


regexp <- "[[:digit:]]+"

# First we assign ann_num_years the numeric value we have extracted from the MEMBERSHIP_LEVEL 
#  column if and only if there is a numeric value present
annual_members[,ann_num_years := str_extract(MEMBERSHIP_LEVEL, regexp)]

# some of them dont have any digits, in that case, replace NA with 1

annual_members[,ann_num_years := ifelse(is.na(ann_num_years), 1, ann_num_years)]

annual_members <- rbind(annual_members[,.(ID_DEMO, FISCAL_YEAR, ann_num_years)], annual_members[,.(ID_OF_RECORD_WITH_RELATIONSHIP, FISCAL_YEAR, ann_num_years)], use.names=FALSE)[!is.na(ID_DEMO)]
annual_members <- unique(annual_members)


mid_dt <- copy(annual_members)[, `:=`(FY1=FISCAL_YEAR+1, FY2=FISCAL_YEAR+2, FY3=FISCAL_YEAR+3, FY4=FISCAL_YEAR+4)]
mid_dt_1 <- melt.data.table(mid_dt[ann_num_years == 5], measure.vars = c("FISCAL_YEAR", "FY1", "FY2", "FY3", "FY4"), variable.name = "yr", value.name = "FISCAL_YEAR")
mid_dt_2 <- melt.data.table(mid_dt[ann_num_years == 4], measure.vars = c("FISCAL_YEAR", "FY1", "FY2", "FY3"), variable.name = "yr", value.name = "FISCAL_YEAR")
mid_dt_3 <- melt.data.table(mid_dt[ann_num_years == 3], measure.vars = c("FISCAL_YEAR", "FY1", "FY2"), variable.name = "yr", value.name = "FISCAL_YEAR")
mid_dt_4 <- melt.data.table(mid_dt[ann_num_years == 2], measure.vars = c("FISCAL_YEAR", "FY1"), variable.name = "yr", value.name = "FISCAL_YEAR")
mid_dt_5 <- melt.data.table(mid_dt[ann_num_years == 1], measure.vars = c("FISCAL_YEAR"), variable.name = "yr", value.name = "FISCAL_YEAR")

annual_members_per_yr <- rbind(mid_dt_1[,.(ID_DEMO, FISCAL_YEAR)], mid_dt_2[,.(ID_DEMO, FISCAL_YEAR)], mid_dt_3[,.(ID_DEMO, FISCAL_YEAR)], 
                               mid_dt_4[,.(ID_DEMO, FISCAL_YEAR)], mid_dt_5[,.(ID_DEMO, FISCAL_YEAR)])
annual_members_per_yr <- unique(annual_members_per_yr)

# getting the window below 2021
annual_members_per_yr <- annual_members_per_yr[FISCAL_YEAR <=2020]
annual_members_per_yr[, member_status := 'annual_member']

annual_mid <- data.table(ID_DEMO = unique(annual_members_per_yr$ID_DEMO),
                         '2020' = 'non_member',
                         '2019' = 'non_member',
                         '2018' = 'non_member',
                         '2017' = 'non_member',
                         '2016' = 'non_member')

annual_mid <- melt.data.table(annual_mid, measure.vars = c('2020','2019', '2018', '2017', '2016'), variable.name = "FISCAL_YEAR", value.name = "val")
annual_mid <- annual_mid[, FISCAL_YEAR := as.integer(as.character(FISCAL_YEAR))]
annual_members_per_yr <- merge.data.table(annual_mid, annual_members_per_yr,
                                          by=c("ID_DEMO", "FISCAL_YEAR"),
                                          all.x = TRUE)
annual_members_per_yr[, member_status := ifelse(is.na(member_status), 'non_member', member_status)]
annual_members_per_yr[, val:=NULL]

# some people have purchased both annual and life in which case, they will
# be nonmembers in vice versa

annual_life_per_yr <- merge.data.table(life_members_per_yr, annual_members_per_yr, by = c("ID_DEMO", "FISCAL_YEAR"))

annual_life_per_yr <- annual_life_per_yr[, member_status := ifelse(member_status.x=='life_member' & member_status.y=='non_member', 
                                                                   'life_member',
                                                                   ifelse(member_status.x=='non_member' & member_status.y=='annual_member',
                                                                          'annual_member',
                                                                          ifelse(member_status.x=='life_member' & member_status.y=='annual_member',
                                                                                 'life_member',
                                                                                 'non_member')))]
annual_life_per_yr <- annual_life_per_yr[,.(ID_DEMO, FISCAL_YEAR, member_status)]


# group 3 :non members 
# these are those who are in indy file but not in above two groups
non_members <- indy_info_main[!(ID_DEMO %in% annual_members_per_yr$ID_DEMO) & 
                                !(ID_DEMO %in% life_members_per_yr$ID_DEMO),.(ID_DEMO)]


records_minus5 = copy(non_members)[, FISCAL_YEAR := current_fy-5]
records_minus4 = copy(non_members)[, FISCAL_YEAR := current_fy-4]
records_minus3 = copy(non_members)[, FISCAL_YEAR := current_fy-3]
records_minus2 = copy(non_members)[, FISCAL_YEAR := current_fy-2]
records_minus1 = copy(non_members)[, FISCAL_YEAR := current_fy-1]


non_members_per_yr <- rbind(records_minus5,records_minus4,records_minus3,records_minus2,records_minus1)
non_members_per_yr[, member_status := 'non_member']

# now, we join all these groups of interest to form main df

main_df <- rbind(non_members_per_yr, 
                 life_members_per_yr, annual_members_per_yr)

# we now remove the ID-s in annual_life because thode records would be inadccurate
# in the main_df, we will jin them back

main_df <- main_df[! ID_DEMO %in% annual_life_per_yr$ID_DEMO]

# we join back the annual_life accurate ones

main_df <- rbind(main_df, annual_life_per_yr)



membership_LastYear <- main_df[FISCAL_YEAR == current_fy-1][, membership_LastYear := member_status][,.(ID_DEMO, membership_LastYear)]
membership_TwoYearsAgo <- main_df[FISCAL_YEAR == current_fy-2][, membership_TwoYearsAgo := member_status][,.(ID_DEMO, membership_TwoYearsAgo)]


# adding compensation flag to find out who became member because of compensation

#we now remove last yr data to get the training data.

# main_df_train <- main_df[FISCAL_YEAR!=current_fy-1]

# for full data, we do not do the above step
main_df_train <- main_df

main_df_train <- dcast.data.table(main_df_train, ID_DEMO~member_status, fun=length) 

#if everything is accurate, the tot of every record should be 4
test <- copy(main_df_train)[, tot := annual_member+ life_member+ non_member]


# Join the LastYear and TwoYearsAgo member_status columns
main_df_train = merge.data.table(main_df_train, membership_LastYear, by = "ID_DEMO", all.x=TRUE) 
main_df_train = merge.data.table(main_df_train, membership_TwoYearsAgo, by = "ID_DEMO", all.x=TRUE) 


# we will look at those whose received complimentary mebership and remove them from our data

comp_alumni <- membership_last5[grepl("Comp",MEMBERSHIP_LEVEL)][FISCAL_YEAR==2020 | FISCAL_YEAR==2019,]
comp_alumni <- unique(comp_alumni[, .(ID_DEMO)])

main_df_train <- main_df_train[!(ID_DEMO %in% comp_alumni$ID_DEMO)]


current_members <- rbind(indy_info_main[,.(ID_DEMO, MEMBERSHIP_TYPE_CODE, MEMBERSHIP_STATUS_CODE)], indy_info_main[,.(ID_SPOUSE, MEMBERSHIP_TYPE_CODE, MEMBERSHIP_STATUS_CODE)], use.names=FALSE)
non_current_members <- current_members[MEMBERSHIP_STATUS_CODE!='C', .(ID_DEMO)]

final_df <- merge.data.table(main_df_train, non_current_members)
# Emails & Events --------------------------------------------------------------

# setwd("D:/Group Folder/Data")
# emails_events <- read_csv('pred_emails_events.csv')
#emails_events <- fread('ML files/pred_emails_events.csv')
# if full data, the above df will be full email events data

emails_events <- fread('ML files/all_emails_events.csv')




emails_events_member <- merge.data.table(final_df, emails_events, by = c('ID_DEMO'), all.x=TRUE)
emails_events_member[is.na(emails_events_member)] = 0


# Engagement --------------------------------------------------------------

# Load the cleaned engagement file
# eng_cleaned <- read.csv("D:/Group Folder/Data/Cleaned data sets/engagement_cleaned.csv",
#                         header = TRUE)
eng_cleaned <- fread("engagement_cleaned.csv", header = TRUE)


# Average UMN Annual engagement scores for last 5 years for each Alumni - training
#last_5_avg_eng <- eng_cleaned[(ID_DEMO %in% main_df_train$ID_DEMO) & (YEAR_FISCAL >= previous_fy - 5) & (YEAR_FISCAL != previous_fy)]
# if full data, the aobve line chnages
last_5_avg_eng <- eng_cleaned[(ID_DEMO %in% final_df$ID_DEMO) & (YEAR_FISCAL >= previous_fy - 5)]

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
training_df <- merge.data.table(last_5_avg_eng, emails_events_member, by = "ID_DEMO")



#fwrite(training_df, "Ml files/new_member_training.csv", row.names = FALSE)
fwrite(training_df, "Ml files/new_member_all_data.csv", row.names = FALSE)

