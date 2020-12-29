#set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\fresh_data')


## Uncomment lines with filter of fisca_year < 2020 for getting training data

## Libraries used

library(data.table)
library(lubridate)

## Variables used

# current fiscal year = 2021
current_fy = ifelse(month(Sys.Date()) > 6, year(Sys.Date()) + 1, 
                    year(Sys.Date())) 


# getting individual data

indy <- fread('individual_info_cleaned.csv')
indy <- indy[, .(ID_DEMO, MEMBERSHIP_STATUS_CODE, MEMBERSHIP_TYPE_CODE)]


# Getting Emails data 
emails <- fread('emails_cleaned.csv')

# inner join so we only have data on those we have demographic data 
emails_indy <- merge.data.table(emails, indy, by = 'ID_DEMO')

emails_indy[, ctr := ifelse(CODE_OUTCOME == 'CL', 1, 0)][,click_open := ifelse(CODE_OUTCOME == 'CL' | CODE_OUTCOME == 'OE', 1, 0)]


emails_indy_2 <- emails_indy[,.(total_possible = .N,total_clicked = sum(click_open)),by=.(MEMBERSHIP_STATUS_CODE, MEMBERSHIP_TYPE_CODE)][, click_or_open := total_clicked / total_possible]


