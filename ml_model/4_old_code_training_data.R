#set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\fresh_data')

## Libraries used

#library(tidyverse)
#library(lubridate)
library(data.table)
library(stringr)
#library(dplyr)

## variables used

# current fiscal year
current_fy = as.integer(ifelse(month(Sys.Date()) > 6, year(Sys.Date()) + 1, 
                     year(Sys.Date())))
previous_fy = current_fy - 1


# 2 Data Cleaned Files

indy_info_main <- fread('individual_info_cleaned.csv')
membership_main <- fread('membership_cleaned.csv')


membership <- copy(membership_main)


# Group 1: People who purchased membership in last 5 years
membership_df_last5 <- membership[
  FISCAL_YEAR>current_fy-6 & FISCAL_YEAR < current_fy, ]


# we spread this out so that we get a row per fiscal year eventually

mem_last5_spread <- dcast.data.table(membership_df_last5,
                                     ID_DEMO~FISCAL_YEAR,
                                     value.var = 'AMOUNT_PAID',
                                     fun.aggregate = sum)
mem_last5_per_yr <- melt.data.table(mem_last5_spread, 
                                    measure.vars = c("2016", "2017", "2018", "2019", "2020"),
                                    variable.name = "FISCAL_YEAR", value.name = "AMOUNT_PAID")


membership_df_last5 <- membership_df_last5[, FISCAL_YEAR:= as.factor(FISCAL_YEAR)]

setkey(mem_last5_per_yr, ID_DEMO, FISCAL_YEAR)
setkey(membership_df_last5, ID_DEMO, FISCAL_YEAR)

records_mem_last5 <- membership_df_last5[mem_last5_per_yr]
records_mem_last5 <- records_mem_last5[,AMOUNT_PAID := i.AMOUNT_PAID][,.(ID_DEMO, AMOUNT_PAID, MEMBERSHIP_LEVEL, FISCAL_YEAR)][, type:="mem_last5"]

# Group2 : People who paid 0 amount or got a complimentary membership
# $0 total spent 

paid_0_last5 <- mem_last5_per_yr[, .(total_spend = sum(AMOUNT_PAID)),by = ID_DEMO][total_spend==0][,.(ID_DEMO)]



# Group 3: people who were lifetime members before 2015

life_before_5 = copy(indy_info_main)[MEMBERSHIP_TYPE_CODE == "L", .(ID_DEMO)][
  !ID_DEMO %in% membership_df_last5$ID_DEMO] 


records_minus5 <- copy(life_before_5)[, FISCAL_YEAR := 2016]

records_minus4 <- copy(life_before_5)[, FISCAL_YEAR := 2017]

records_minus3 <- copy(life_before_5)[, FISCAL_YEAR := 2018]

records_minus2 <- copy(life_before_5)[, FISCAL_YEAR := 2019]

records_LastYear <- copy(life_before_5)[, FISCAL_YEAR := 2020]

records_life_before_5 <- rbind(records_minus5,records_minus4,records_minus3,records_minus2,records_LastYear)
records_life_before_5[,`:=`(AMOUNT_PAID = NA,
                                  MEMBERSHIP_LEVEL = NA,
                                  type ="life_before5")]


# Group 4: people who have never been members (we conside people who have annual membership before last 5 yrs as new members)
new_members <- indy_info_main[!(ID_DEMO %in% membership_df_last5$ID_DEMO) & 
                                         !(ID_DEMO %in% life_before_5$ID_DEMO),.(ID_DEMO)]


records_minus5 = copy(new_members)[, FISCAL_YEAR := current_fy-5]
records_minus4 = copy(new_members)[, FISCAL_YEAR := current_fy-4]
records_minus3 = copy(new_members)[, FISCAL_YEAR := current_fy-3]
records_minus2 = copy(new_members)[, FISCAL_YEAR := current_fy-2]
records_LastYear = copy(new_members)[, FISCAL_YEAR := current_fy-1]


records_new_members <- rbind(records_minus5,records_minus4,records_minus3,records_minus2,records_LastYear)
records_new_members[,`:=`(AMOUNT_PAID = NA,
                                     MEMBERSHIP_LEVEL = NA,
                                     type="non_members")][,.(ID_DEMO, AMOUNT_PAID, MEMBERSHIP_LEVEL, FISCAL_YEAR, type)]

# now, we join all thes groups of interest to form main df

main_df <- rbind(records_life_before_5, records_new_members, records_mem_last5)



test <-main_df[, .(num = .N), .(ID_DEMO, FISCAL_YEAR)]
# feature engineering

# finalizing the ttype of members (annual, life, nonmember)

main_df[,member_status := ifelse(is.na(MEMBERSHIP_LEVEL), 'non-member', 
                                ifelse(str_detect(MEMBERSHIP_LEVEL, 'Life'), 'life-member', 'annual-member'))]
main_df[,member_status := ifelse(type == "life_before5", "life-member", member_status)]


# detecting the number of years the annual membership is purchased for

regexp <- "[[:digit:]]+"

# First we assign ann_num_years the numeric value we have extracted from the MEMBERSHIP_LEVEL 
#  column if and only if there is a numeric value present
main_df[,ann_num_years := str_extract(MEMBERSHIP_LEVEL, regexp)]


# there are UMAA Annual  Membership (Joint Life Install Plan F6)	 and E6 changing that to just 1 year for now
main_df[,ann_num_years := ifelse(str_detect(MEMBERSHIP_LEVEL, 'F6') | str_detect(MEMBERSHIP_LEVEL, 'E6') , 1, ann_num_years)]

# this is for those wher the above regex did not work
main_df[, ann_num_years:=ifelse(!is.na(MEMBERSHIP_LEVEL) & is.na(ann_num_years) & member_status!="life-member", 1, ann_num_years)]

main_df[, FISCAL_YEAR:= as.numeric(as.character(FISCAL_YEAR))]




# accounting for multi year purchase, if a person purchases 2 yr membership in 2016, they should be a member in 2017 and not a member in 2018
main_df[order(FISCAL_YEAR), `:=`(prev1 = dplyr::lag(ann_num_years),
                    prev2 = dplyr::lag(ann_num_years,2),
                    prev3 = dplyr::lag(ann_num_years,3),
                    prev4 = dplyr::lag(ann_num_years,4)),by=ID_DEMO]
 

# Replace NA's with zero for mutation step to work
main_df[,`:=`(prev1= tidyr::replace_na(prev1, 0),
              prev2= tidyr::replace_na(prev2, 0),
              prev3= tidyr::replace_na(prev3, 0),
              prev4= tidyr::replace_na(prev4, 0))]

main_df[,`:=`(prev1= as.character(prev1),
              prev2= as.character(prev2),
              prev3= as.character(prev3),
              prev4= as.character(prev4))]
# Fill the member_status column based on multi-year purchases
main_df[,member_status := ifelse((prev1 == 2 | prev1 == 3 | prev1 == 4 | prev1 == 5), "annual-member", member_status)]
main_df[,member_status := ifelse((prev2 == 3 | prev2 == 4 | prev2 == 5), "annual-member", member_status )]
main_df[,member_status := ifelse((prev3 == 4 | prev3 == 5), "annual-member", member_status )]
main_df[,member_status := ifelse((prev4 == 5), "annual-member", member_status )]

# Remove columns used in this engineering
main_df[, c("prev1", "prev2", "prev3", "prev4") := NULL][]




## Life:
# Fill Lifetime members' member_status with "life-member" for years after they made their lifetime purchase

main_df[order(FISCAL_YEAR), `:=`(prev1 = lag(member_status),
                                           prev2 = lag(member_status,2),
                                           prev3 = lag(member_status,3),
                                           prev4 = lag(member_status,4)),by=ID_DEMO]

# Replace NA's with zero for mutation step to work
main_df[,`:=`(prev1= tidyr::replace_na(prev1, 0),
              prev2= tidyr::replace_na(prev2, 0),
              prev3= tidyr::replace_na(prev3, 0),
              prev4= tidyr::replace_na(prev4, 0))]

main_df[,`:=`(prev1= as.character(prev1),
              prev2= as.character(prev2),
              prev3= as.character(prev3),
              prev4= as.character(prev4))]



# Fill the member_status column based on lifetime purchases
main_df[,member_status := ifelse(prev1 == "life-member" |
                                                     prev2 == "life-member" | 
                                                     prev3 == "life-member" | 
                                                     prev4 == "life-member", "life-member", member_status)]



# Remove columns used in this engineering
main_df[, c("prev1", "prev2", "prev3", "prev4") := NULL][]


### E - Rollup Prep

# Remove duplicates
main_df <- unique(main_df, by = c("ID_DEMO", "FISCAL_YEAR"))


# Add a life_year column for rollup
main_df[, life_yr := ifelse(grepl("Life",MEMBERSHIP_LEVEL), FISCAL_YEAR, 0)]

# Handle installments
main_df[, ann_num_years := ifelse(grepl("Install",MEMBERSHIP_LEVEL), 0, ann_num_years)]



### F - Rollup

TARGET_YR_GLOBAL = current_fy - 1
TRAIN_DATA_YRS = 4

# Get the target values
membership_LastYear <- copy(main_df)[FISCAL_YEAR == current_fy - 1, ][, membership_LastYear := member_status][,.(ID_DEMO, membership_LastYear)]

membership_TwoYearsAgo <- copy(main_df)[FISCAL_YEAR == current_fy - 1, ][, membership_TwoYearsAgo := member_status][,.(ID_DEMO, membership_TwoYearsAgo)]

# Remove membership_LastYear to not include it in the rollup counts
membership_rollup <- main_df[FISCAL_YEAR != current_fy - 1]



# Add non-mem years
membership_rollup[, nonmem_num_years := ifelse(member_status == "non-member", 1, 0)]

# aggregate 
membership_rollup_agg <- membership_rollup[.(annual_years = sum(as.numeric(ann_num_years)),
            life_years = TARGET_YR_GLOBAL - sum(life_yr),
            nonmem_years = sum(nonmem_num_years),
            life_before_window_flag = sum(life_before_window)), by=.(ID_DEMO)]

membership_rollup_agg[, `:=`(annual_years = ifelse(annual_years > TRAIN_DATA_YRS, TRAIN_DATA_YRS, annual_years),
                                                 life_years = ifelse(life_years == TARGET_YR_GLOBAL, 0, life_years),
                                                 life_years = ifelse(life_before_window_flag == TRAIN_DATA_YRS, 4,life_years))]




