#set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\fresh_data')


## Libraries used

library(data.table)
library(lubridate)

## Variables used

# current fiscal year = 2021
current_fy = ifelse(month(Sys.Date()) > 6, year(Sys.Date()) + 1, 
                     year(Sys.Date())) 


# getting individual data

indy <- fread('individual_info_cleaned.csv')
indy <- indy[, .(ID_DEMO, MEMBERSHIP_STATUS_CODE)]


# Getting Emails data 
emails <- fread('emails_cleaned.csv')


# inner join so we only have data on those we have demographic data 
emails_indy <- merge.data.table(emails, indy, by = 'ID_DEMO')

# Select imp variables and imp categories
emails_indy <- emails_indy[,.(ID_DEMO, DATE_CONTACT, CODE_PATH, CODE_OUTCOME, broad_cat)][broad_cat %in% c('Learning', 'Legislature', 'Social', 'Sports'), ]


# create new columns
emails_indy[, ctr := ifelse(CODE_OUTCOME == 'CL', 1, 0)][,click_open := ifelse(CODE_OUTCOME == 'CL' | CODE_OUTCOME == 'OE', 1, 0)]


# Each person's avg click_open rate per category - Aggregation
clickthru_rates <- emails_indy[,.(total_possible = .N,
                             total_clicked = sum(click_open)),
                          by=.(ID_DEMO, broad_cat)][, click_or_open := total_clicked / total_possible]
 

# doing a pivot operation, so each person has a single row

clickthru_rates_spread <- dcast.data.table(clickthru_rates, ID_DEMO ~ broad_cat, value.var = 'click_or_open')




# Computing each person's avg ctr across all categories
clickthru_rates_general <- emails_indy[,.(total_possible = .N,
                                     total_clicked = sum(click_open)),
                                  by=ID_DEMO][, general_ctr := total_clicked / total_possible][, .(ID_DEMO,general_ctr)]



# Combine per-category click_open rates with a general ctr
clickthru_rates_spread <- merge.data.table(clickthru_rates_spread, clickthru_rates_general, by = 'ID_DEMO')
  
clickthru_rates_spread <- dplyr::rename_at(clickthru_rates_spread,  dplyr::vars(-ID_DEMO), ~ paste0(., '_emails'))



# Events data preparation

events_main <- fread('events_cleaned.csv')

# filtering by year 2014
events <- copy(events_main)[YEAR_FISCAL > 2014, .(ID_DEMO, DATE_EVENT, YEAR_FISCAL, broad_cat, NBR_GROUP)]

# merging with individual data to get data for those we have dempgraphic info
events <- merge.data.table(events, indy, by = 'ID_DEMO') 


# Adding a column to show how many of each category a person went to
events_group <- events[, .(total_type_person=.N), by=.(ID_DEMO, broad_cat)]



# Transpose operation - getting each category as a column
events_spread <- dcast.data.table(events_group, ID_DEMO ~ broad_cat, value.var = 'total_type_person')


# Counting total events

events_total <- events[, .(total_type_person=.N), by=.(ID_DEMO)]

events_spread <- merge.data.table(events_spread, events_total, by = "ID_DEMO")
events_spread <-  dplyr::rename_at(events_spread,  dplyr::vars(-ID_DEMO), ~ paste0(., '_events'))


# Per-person --------------------------------------------------------------

# Connecting emails & events
main_df <- merge.data.table(clickthru_rates_spread, events_spread, by = c('ID_DEMO')) 
main_df <- merge.data.table(main_df, indy, by = 'ID_DEMO')
main_df[is.na(main_df)] = 0
main_df <- main_df[,MEMBERSHIP_STATUS_CODE:=NULL]
  
fwrite(main_df, file='./ML files/all_emails_events.csv')




