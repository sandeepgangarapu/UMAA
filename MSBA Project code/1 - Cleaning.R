## set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\new')


## Libraries used

# library(reticulate)
library(dplyr)
library(naniar)
library(ggplot2)
library(lubridate)
library(data.table)
library(tidyverse)
library(dtplyr)
library(stringr)
require(scales)

## variable used

# current fiscal year
current_fy = ifelse( month(Sys.Date()) > 6, year(Sys.Date()) + 1, 
                     year(Sys.Date())) 


# Academics --------------------------------------------------------------
#py_run_file('D:/Group Folder/Code/Read academic.py')
# academic = read.csv('D:/Group Folder/Data/academic.csv')
academic = read.csv('academic.csv')



# total obs, 1,699,908
# unique id, 617,372
length(unique(academic$ID_DEMO))

# keep only one records per individual

# use this one
# we only have 617372 obs, unique!
academic_group <- academic %>%
  group_by(ID_DEMO) %>%
  mutate(fre = n()) %>% 
  arrange(desc(YEAR_GRAD)) %>% 
  select(ID_DEMO,CODE_CLG_QUERY,YEAR_GRAD,CODE_UM_DEGREE_LEVEL,fre)

# create a new column indicating the word count of the CODE_CLG
academic_group$leng = str_count(academic_group$CODE_CLG_QUERY)

# remomve duplicates
academic_dedup = academic_group %>% filter(!((leng == 4) & (fre > 1)))

# for people who appear a lot of times, random keep one
# window rank creaation
academic_dedup = academic_dedup %>% group_by(ID_DEMO) %>% mutate(rownum = row_number()) 

# select final
academic_final = academic_dedup %>% filter(rownum == 1)

#  check missing values for each column
# missing are all missing...
sapply(academic_dedup, function(x) sum(is.na(x)))

# missing values
# remove NA values
# all these records missing YEAR_GRAD, also missing other fields
academic_null = academic_final[which(is.na(academic_final$YEAR_GRAD)),]

# keep non-missing rows
academic_export = academic_final[which(!is.na(academic_final$YEAR_GRAD)),]
sum(is.na(academic_export))

# remove those unknown people whose YEAR_GRAD is 9999
academic_export = academic_export %>% filter(!(YEAR_GRAD == 9999))

# remove those people whose YEAR_GRAD is great than the current year
academic_export = academic_export %>% filter(!(YEAR_GRAD >= year(Sys.Date())))

# export
# write.csv(academic_export %>% 
#             select(ID_DEMO,YEAR_GRAD,CODE_UM_DEGREE_LEVEL),
#           'D:\\Group Folder\\Data\\Cleaned data sets\\Academic_cleaned.csv',row.names = FALSE)
write.csv(academic_export %>% 
            select(ID_DEMO,YEAR_GRAD,CODE_UM_DEGREE_LEVEL),
          'academic_cleaned.csv',row.names = FALSE)




# Membership --------------------------------------------------------------
# py_run_file('D:/Group Folder/Code/Read membership.py')
# membership = read.csv("D:/Group Folder/Data/membership.csv")
membership = read.csv("membership.csv")
membership_clean = membership

# ID_DEMO 
membership_clean = membership_clean %>% filter(ID_DEMO != "4B0BE2AB589CDD2B8ED0193018EB3015")

# APPEAL_CODE
# Replace rows without appeal codes but with membership level with "None"
membership_clean = membership_clean %>% 
  mutate(APPEAL_CODE = ifelse((APPEAL_CODE == "") & (MEMBERSHIP_LEVEL != ""),"None",as.character(APPEAL_CODE)))
# Remove rows that remain without appeal codes or mamberhsip levels
membership_clean = membership_clean %>% filter(APPEAL_CODE != "")

# FORM_OF_PAYMENT - EDA ONLY
# Group these better later if needed

# AMOUNT_PAID
# remove donations (> 1000) and refunds (< 0)
membership_clean = membership_clean %>% filter(membership_clean$AMOUNT_PAID >= 0 & membership_clean$AMOUNT_PAID < 1000)

# TRANSACTION_DATE
# transform to date, add fiscal_year column
membership_clean$TRANSACTION_DATE = as.Date(membership_clean$TRANSACTION_DATE, format = "%m/%d/%Y")
membership_clean$FISCAL_YEAR = ifelse(month(membership_clean$TRANSACTION_DATE) >= 7, 
                                      year(membership_clean$TRANSACTION_DATE)+1, 
                                      year(membership_clean$TRANSACTION_DATE))

# MEMBERSHIP_LEVEL
# keep only rows pertaining to annual, lifetime memberships
membership_clean = membership_clean %>% filter(grepl("Annual", MEMBERSHIP_LEVEL) | grepl("Life", MEMBERSHIP_LEVEL))

# Write CSV
# write.csv(membership_clean, "D:/Group Folder/Data/Cleaned data sets/membership_cleaned.csv",row.names = FALSE)

write.csv(membership_clean, "membership_cleaned.csv",row.names = FALSE)


# Individual info --------------------------------------------------------------

# py_run_file('D:/Group Folder/Code/Read individual_info.py')
# individual_info = read.csv('D:/Group Folder/Data/individual_info.csv')
individual_info = read.csv('individual_info.csv')

# KEEP ONLY WHAT IS USEFUL FOR CLUSTERING

individual_info_clean <- individual_info[,c("ID_DEMO",
                                              "ID_SPOUSE",
                                              "MARITAL_STATUS",
                                              "GENDER",
                                              "BIRTH_YEAR",
                                              "DEATH_YEAR",
                                              "AGE",
                                              "ZIP_CODE",
                                              "IN_TC_METRO_AREA",
                                            #  "HOUSEHOLD_INCOME",
                                              "IS_TC_GRAD",
                                              "MOST_RECENT_COLLEGE",
                                              "MOST_RECENT_GRAD_YEAR",
                                              "ATHLETIC_INTEREST",
                                              "TRAVEL_INTEREST",
                                              "MEMBERSHIP_TYPE_CODE",
                                              "MEMBERSHIP_STATUS_CODE",
                                              "FIRST_EVER_START_DATE",
                                              "EXPIRATION_DATE_OF_MOST_RECENT_MEMBERSHIP")]

# Replace '' with NA in the dataframe
individual_info_clean[individual_info_clean==''] <- NA

# Keep all those who are TC GRAD

# 39,615 records are eliminated.
# 577,757 remain.

individual_info_clean <-individual_info_clean %>% filter(IS_TC_GRAD=='Y')


# Death year treatment

# Keep all records that is NA or 9999
# 
# 57,961 are eliminated.
# 519,796 records remain.

individual_info_clean <-individual_info_clean %>% filter((DEATH_YEAR==9999)|(is.na(DEATH_YEAR)))



# Inspect DEATH_YEAR = 9999

# These are the ones who's BIRTH_YEAR has to be determined.
# 
# 25,545 (~5%) of the records have a value of 9999.
# 
# We impute values for BIRTH_YEAR based on MOST_RECENT_GRAD_YEAR if the values are NA.
# 
# 202 records are removed as they have no birth_year or most_recent_grad_year information.
# 
# We lose 1% of the records.


individual_info_clean <- individual_info_clean%>% 
  mutate(BIRTH_YEAR = ifelse(
    ((DEATH_YEAR==9999)&is.na(BIRTH_YEAR)&(MOST_RECENT_GRAD_YEAR==9999)),NA,
    ifelse((is.na(BIRTH_YEAR)&(DEATH_YEAR==9999)),
           MOST_RECENT_GRAD_YEAR-23,
           BIRTH_YEAR)) )



# Calculate age using BIRTH_YEAR

individual_info_clean <- individual_info_clean %>% 
  mutate(AGE = year(Sys.Date()) - BIRTH_YEAR)

# Age

# Keep age between 0 to 100. 504,573 records remain


individual_info_clean <- individual_info_clean %>% filter((AGE %in% NA) | ((AGE >= 0) & (AGE  <= 100)))


# Consider all the remaining members as living

individual_info_clean$DEATH_YEAR <- na_if(individual_info_clean$DEATH_YEAR,9999)


# Impute AGE using mean value for AGE
individual_info_clean <- individual_info_clean%>% mutate(AGE = ifelse((AGE %in% NA), mean(AGE, na.rm = TRUE),AGE) )

# ZIPCODE imputation

individual_info_clean$ZIP_CODE <- as.character(individual_info_clean$ZIP_CODE)


income_zipcode <- read_csv('income_zipcode.csv')
income_zipcode$zip_code = as.character(income_zipcode$zip_code)

# Join zipcode file 
individual_info_clean = individual_info_clean %>% left_join(income_zipcode, by = c("ZIP_CODE" = "zip_code"))

# Add new column to detect in TC area
individual_info_clean = individual_info_clean %>% mutate(IN_TC_METRO_AREA = ifelse(TC_AREA == 1, "Y", "N"))

# Add new column for Household Income
individual_info_clean = individual_info_clean %>% mutate(HOUSEHOLD_INCOME = ifelse(!is.na(HOUSEHOLD_INCOME), HOUSEHOLD_INCOME,
                                                                                       ifelse(is.na(age_25), NA, 
                                                                                              ifelse(AGE < 26, age_25, 
                                                                                                     ifelse(AGE < 45, age_25_44,
                                                                                                            ifelse(AGE < 65, age_45_64, age_65))))))


# Remove columns that are not required. 484,950 records remain

individual_info_clean <- individual_info_clean %>% select(-c(TC_AREA, County, age_25, age_25_44, age_45_64,age_65))


individual_info_clean<- individual_info_clean%>%filter(!((IN_TC_METRO_AREA %in% NA) & (ZIP_CODE%in%NA)))


# Household income

individual_info_clean <- individual_info_clean %>% group_by(AGE)%>%mutate(HOUSEHOLD_INCOME = ifelse(is.na(HOUSEHOLD_INCOME), mean(HOUSEHOLD_INCOME, na.rm=TRUE),HOUSEHOLD_INCOME ) )


# IN TC METRO AREA

# Replace NA with NO


individual_info_clean <- individual_info_clean %>% mutate(IN_TC_METRO_AREA = ifelse(IN_TC_METRO_AREA %in% NA, "N","Y") )

# Convert FIRST_EVER_START_DATE, EXP_DATE_LAST_MEMBERSHIP to date



individual_info_clean$FIRST_EVER_START_DATE <- mdy(individual_info_clean$FIRST_EVER_START_DATE)

individual_info_clean$EXP_DATE_LAST_MEMBERSHIP <- mdy(individual_info_clean$EXPIRATION_DATE_OF_MOST_RECENT_MEMBERSHIP)

# Keep only those records that are required

individual_info_clean <- individual_info_clean %>% select(-c(DEATH_YEAR, IS_TC_GRAD))



# Gender

# Remove Gender = U
# We lose 130 records
# 484,712 records remain.
individual_info_clean <- individual_info_clean %>% filter(GENDER != "U")



# write.csv(individual_info_clean, "D:/Group Folder/Data/Cleaned data sets/individual_info_cleaned.csv", row.names = FALSE)
write.csv(individual_info_clean, "individual_info_cleaned.csv", row.names = FALSE)

# Engagement --------------------------------------------------------------

# read raw engagement score file
# eng <- read.csv("D:\\Group Folder\\Data\\engagement.csv", header = TRUE)
eng <- read.csv("engagement.csv", header = TRUE)

# remove blank records
eng_no_blank <- eng[!is.na(eng$YEAR_FISCAL),]

# remove fiscal year 2014 records because all the scores are 0's
eng_cleaned <- eng_no_blank[eng_no_blank$YEAR_FISCAL != 2014,]

# save the cleaned engagement score into a csv file
# write.csv(eng_cleaned, "D:/Group Folder/Data/Cleaned data sets/engagement_cleaned.csv", row.names = FALSE)
write.csv(eng_cleaned, "engagement_cleaned.csv", row.names = FALSE)


# Events --------------------------------------------------------------

#py_run_file('D:/Group Folder/Code/Read events.py')



events <- fread('events.csv')
events <- events %>% 
  mutate(DATE_EVENT = mdy(DATE_EVENT)) %>%
  mutate(YEAR_FISCAL = ifelse(month(DATE_EVENT) > 6, year(DATE_EVENT) + 1, year(DATE_EVENT))) %>%
  mutate(NAME_GROUP = str_to_lower(events$NAME_GROUP))




### Applying broad categories to email names
events_distinct <- events %>% distinct(NBR_GROUP, NAME_GROUP)
network <- c('day of service|fundraiser|mcan|epn|connect|summit|leader|network|event|alumni|job|career|aaw')
solicit <- c('gtmd|benefit|deal|senior|junior|student|opt|new grad|redeem|give|gift|appeal|donor|solicitmembership|survey|spring|fall|promo|offer|renew')
social <- c('mnu|behind|bts|dta|brunch|interest|date|gradfest|engage|resched|postpo|valleyfair|invite|holiday|matchmaker|gala|birthday|farewell|volunteer|potluc|picnic|night|reunion|social|happy hour|mixer|brew|wine|tour|travel|movie|breakfast|lunch|dinner|meet|skiumania|service|trip|gathering|awards|travel|celebration|party|ceremony|bike|ski-u')
laws <- c('affordable care|dc|candid|state|climate|legis|laws|election|senate|vote|board|counc|capitol|voting|forum|pre-ele|advocacy')
sports <- c('btn|goldy|fleck|coach|sparks|sports|football|basketball|twins|bowl|game|hockey|golf|soccer|baseball|volleyball|halftime|timber|wild|rockies|gophers|lynx|tickets|homecoming|ncaa|frozen|athletics|bball|gymnastics|giants|big ten')
regions <- c('swfl|charlotte|france|japan|phoenix|beijing|vietnam|milwaukee|korea|new jersey|asia|florida|arizona|california|washington|seattle|india|vietnam|colorado|atlanta|internation|china|london|nyc|chicago|new mexico|houston')
learning1 <- c('webinar|negotiate|branding|web|learning|coursera|minnecollege|minne-coll|minne coll')
learning2 <- c('librar|talk|web|learning|coursera|lecture|speaker')




events_distinct <- events_distinct %>% 
  mutate(broad_cat = 
    ifelse(grepl(paste(learning1, collapse='|'), NAME_GROUP), 'Learning',
    ifelse(grepl(paste(sports, collapse='|'), NAME_GROUP), 'Sports', 
    ifelse(grepl(paste(social, collapse='|'), NAME_GROUP), 'Social',
    ifelse(grepl(paste(laws, collapse='|'), NAME_GROUP), 'Legislature', 
    ifelse(grepl(paste(learning2, collapse='|'), NAME_GROUP), 'Learning',
    ifelse(grepl(paste(network, collapse='|'), NAME_GROUP), 'Networking','Other'))))))) %>%
  select(-NAME_GROUP)


events <- events %>% inner_join(events_distinct, by = 'NBR_GROUP')
write.csv(events, 'events_cleaned.csv',row.names = FALSE)



# Emails --------------------------------------------------------------


emails <- fread("emails.csv")
emails$DESC_PATH <- str_to_lower(emails$DESC_PATH)




# Correct date format, only received emails, creating FY, updating "clicked"
emails_revised <- emails %>%
  mutate(DATE_CONTACT = mdy(DATE_CONTACT)) %>%
  filter(CODE_OUTCOME != 'BO') %>%
  mutate(YEAR_FISCAL = ifelse(month(DATE_CONTACT) > 6, year(DATE_CONTACT) + 1, year(DATE_CONTACT))) %>% 
  mutate(Status_Clicked = ifelse(CODE_OUTCOME == 'CL' | CODE_OUTCOME == 'OE', 1, 0)) %>%
  filter(YEAR_FISCAL > 2014)


# Just getting distinct emails to create categories
emails_distinct <- emails_revised %>% distinct(CODE_PATH, DESC_PATH)



clothes <- c('shirt|pants|clothes|socks|shopping|apparel|cybermonday|presale')
laws <- c('dc|candid|state|climate|legis|laws|election|senate|vote|board|counc|capitol|voting|forum|pre-ele|advocacy')
sports <- c('btn|mnu|fleck|coach|sparks|sports|football|basketball|twins|bowl|game|hockey|golf|soccer|baseball|volleyball|halftime|timber|wild|rockies|gophers|lynx|tickets|homecoming|ncaa|frozen|athletics|bball|gymnastics|giants|big ten')
regions <- c('swfl|france|japan|phoenix|beijing|vietnam|milwaukee|korea|new jersey|asia|florida|arizona|california|washington|seattle|india|vietnam|colorado|atlanta|internation|china|london|nyc|chicago|new mexico|houston')
learning <- c('minnecollege|minne-|mcan|epn|career|tech|mentor|librar|talk|webinars|web|learning|coursera|lecture|speaker')
solicit <- c('gtmd|deal|opt|new grad|redeem|give|gift|appeal|donor|solicit|membership|survey|at the u|spring|fall|promo|offer|fundraiser|renew')
mass_updates <- c('aaw|system|news|angle|mem|non-|nonmem|announce|report|email|update|birthday')
social <- c('bts|dta|gala|brunch| at | in |suncoast|interest|date|gradfest|engage|resched|postpo|farewell|remind|volunteer|valleyfair|invite|holiday|connect|summit|leader|potluc|picnic|join|night|reunion|social|happy hour|mixer|networking|event|network|brew|wine|tour|travel|movie|breakfast|lunch|dinner|meet|alumni|skiumania|service|trip|gathering|awards|travel|celebration|party|ceremony|bike|ski-u')


emails_distinct <- emails_distinct %>% 
  mutate(broad_cat = 
    ifelse(grepl(paste(clothes, collapse='|'), DESC_PATH), 'Solicitations', 
    ifelse(grepl(paste(laws, collapse='|'), DESC_PATH), 'Legislature', 
    ifelse(grepl(paste(sports, collapse='|'), DESC_PATH), 'Sports', 
    ifelse(grepl(paste(learning, collapse='|'), DESC_PATH), 'Learning', 
    ifelse(grepl(paste(solicit, collapse='|'), DESC_PATH), 'Solicitations', 
    ifelse(grepl(paste(mass_updates, collapse='|'), DESC_PATH), 'Mass Updates', 
    ifelse(grepl(paste(social, collapse='|'), DESC_PATH), 'Social', 'Other'))))))))



emails_distinct <- emails_distinct %>% select(-DESC_PATH)
emails_revised <- emails_revised %>% select(-DESC_PATH, -DESC_OUTCOME)
emails_revised <- merge.data.table(emails_revised, emails_distinct, by = 'CODE_PATH')


fwrite(emails_revised, file="emails_cleaned.csv",row.names = FALSE)
