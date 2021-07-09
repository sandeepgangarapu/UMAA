## set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\july_2021_data\\raw_files')


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
library(DataExplorer)
## variable used

# current fiscal year
current_fy = ifelse( month(Sys.Date()) > 6, year(Sys.Date()) + 1, 
                     year(Sys.Date())) 


# Academics --------------------------------------------------------------
#py_run_file('D:/Group Folder/Code/Read academic.py')
# academic = read.csv('D:/Group Folder/Data/academic.csv')
academic = read.csv('academic.csv')



# Remove obs with grad year 9999
# Filter only graduates
# filter those with missing observations - around 2.53% of people
# convert ID_demo to factor
# code_colg_query is creating problems with duplication 
# and is not the most useful feature in modeling, so we remove the column
academic_clean <- academic %>% mutate(YEAR_GRAD = as.integer(YEAR_GRAD)) %>% 
  filter(YEAR_GRAD < 2022) %>% 
  filter(CODE_GRAD_ATTENDEE == 'G') %>%
  drop_na() %>% 
  mutate(ID_DEMO = factor(ID_DEMO)) %>% 
  select(-CODE_CLG_QUERY) %>% 
  distinct()

# create new columns - number of degrees to indicate the involvement
# with the university
# removing those with more than 4 degrees to remove skewness
# around 1200 people
academic_new <- academic_clean %>% group_by(ID_DEMO) %>% 
  mutate(NUM_DEG = n()) %>% 
  filter(NUM_DEG < 5)


# there are still duplicate records, so we keep the record 
# with highest degree, we keep the one with highest year of graduation
# this is not perfect but it should work

academic_dedup <- academic_new %>% group_by(ID_DEMO) %>% 
  mutate(rank = rank(desc(YEAR_GRAD), ties.method = "first")) %>% 
  filter(rank==1) %>% select(-rank)

academic_final <- academic_dedup %>% 
  select(ID_DEMO,YEAR_GRAD,CODE_UM_DEGREE_LEVEL,
         CODE_MAJOR, CODE_CAMPUS, NUM_DEG)
write.csv(academic_final,'academic_cleaned.csv',row.names = FALSE)

create_report(academic_final, output_file='academic_eda.html')



# Membership --------------------------------------------------------------
membership = read.csv("membership.csv")


# replace form of payment cc with CC
membership_cleaned <- membership
membership_cleaned$FORM_OF_PAYMENT[membership_cleaned$FORM_OF_PAYMENT == 'cc'] <- 'CC'


# AMOUNT_PAID
# remove donations (> 1000) and refunds (< 0)
membership_cleaned = membership_cleaned %>% filter(AMOUNT_PAID >= 0 & AMOUNT_PAID < 1000)



# TRANSACTION_DATE
# transform to date, add fiscal_year column
membership_cleaned$TRANSACTION_DATE = as.Date(membership_cleaned$TRANSACTION_DATE, format = "%m/%d/%Y")
membership_cleaned$FISCAL_YEAR = ifelse(month(membership_cleaned$TRANSACTION_DATE) >= 7, 
                                      year(membership_cleaned$TRANSACTION_DATE)+1, 
                                      year(membership_cleaned$TRANSACTION_DATE))


# MEMBERSHIP_LEVEL
# keep only rows pertaining to annual, lifetime memberships
membership_cleaned <- membership_cleaned %>% filter(grepl("Annual", MEMBERSHIP_LEVEL) | grepl("Life", MEMBERSHIP_LEVEL))

# Replace rows without appeal codes but with membership level with "None"
membership_cleaned <- membership_cleaned %>% 
  mutate(APPEAL_CODE = ifelse((APPEAL_CODE == "") & (MEMBERSHIP_LEVEL != ""),"None",as.character(APPEAL_CODE)))

# remove records whose ID_demo has more than 100 rows
membership_cleaned <- membership_cleaned %>% group_by(ID_DEMO) %>% mutate(tot = n()) %>% 
  filter(tot <100) %>% select(-tot)


write.csv(membership_cleaned, "membership_cleaned.csv",row.names = FALSE)

create_report(membership_cleaned, output_file='membership_eda.html')

# Individual info --------------------------------------------------------------

individual_info = read.csv('individual_info.csv', sep="|")

individual_info_cleaned <- individual_info
# Replace '' with NA in the dataframe
individual_info_cleaned[individual_info_cleaned==''] <- NA


# Keep only those in USA
# Keep only those who are alive
# keep only those whose age is between 0 and 100
# mutate NA of in_tc_metro area to 'N'
individual_info_filter <- individual_info_cleaned %>% 
  filter(COUNTRY == 'USA') %>% 
  filter(is.na(DEATH_YEAR)) %>% 
  filter((AGE %in% NA) | ((AGE >= 0) & (AGE  <= 100)))


# Convert FIRST_EVER_START_DATE, EXP_DATE_LAST_MEMBERSHIP to date


individual_info_filter$FIRST_EVER_START_DATE <- mdy(individual_info_filter$FIRST_EVER_START_DATE)

individual_info_filter$EXP_DATE_LAST_MEMBERSHIP <- mdy(individual_info_filter$EXPIRATION_DATE_OF_MOST_RECENT_MEMBERSHIP)



# Remove Gender = U

individual_info_filter <- individual_info_filter %>% filter(GENDER != "U")

# labeling IN_TC_metro area based on zipcodes start with "55"
individual_info_filter <- individual_info_filter %>% 
  mutate(IN_TC_METRO_AREA = ifelse(grepl("55",ZIP_CODE), "Y", "N"))


# KEEP ONLY WHAT IS USEFUL FOR CLUSTERING

individual_info_clean <- individual_info_filter[,c("ID_DEMO",
                                                    "ID_SPOUSE",
                                                    "ID_HOUSEHOLD",
                                                    "MARITAL_STATUS",
                                                    "GENDER",
                                                    "AGE",
                                                    "IN_TC_METRO_AREA",
                                                    "IS_CURRENT_TC_EMPLOYEE",
                                                    "ATHLETIC_INTEREST",
                                                    "TRAVEL_INTEREST",
                                                    "AFFINITY_NETWORK_INTEREST",
                                                    "WEB_TOPIC_OPT_INS",
                                                    "MEMBERSHIP_TYPE_CODE",
                                                    "MEMBERSHIP_STATUS_CODE",
                                                    "FIRST_EVER_START_DATE",
                                                    "EXPIRATION_DATE_OF_MOST_RECENT_MEMBERSHIP")]



write.csv(individual_info_clean, "individual_info_cleaned.csv", row.names = FALSE)

create_report(individual_info_clean, output_file='individual_eda.html')

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
create_report(eng_cleaned, output_file='engagement_eda.html')


# Events --------------------------------------------------------------

#py_run_file('D:/Group Folder/Code/Read events.py')



events <- read.csv('events.csv', sep="|")
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

create_report(events, output_file='events_eda.html')



# Emails --------------------------------------------------------------


emails <- fread("emails.csv", sep="|")
emails$DESC_PATH <- str_to_lower(emails$DESC_PATH)




# Correct date format, only received emails, creating FY, updating "clicked"

emails_revised <- emails[, DATE_CONTACT := mdy(DATE_CONTACT)]
emails_revised <- emails_revised[CODE_OUTCOME != 'BO']
emails_revised <- emails_revised[, YEAR_FISCAL := ifelse(month(DATE_CONTACT) > 6, year(DATE_CONTACT) + 1, year(DATE_CONTACT))]
emails_revised <- emails_revised[, Status_Clicked := ifelse(CODE_OUTCOME == 'CL' | CODE_OUTCOME == 'OE', 1, 0)]
emails_revised <- emails_revised[YEAR_FISCAL > 2014]


# Just getting distinct emails to create categories

emails_distinct <- unique(emails_revised, by = c("CODE_PATH", "DESC_PATH"))




clothes <- c('shirt|pants|clothes|socks|shopping|apparel|cybermonday|presale')
laws <- c('dc|candid|state|climate|legis|laws|election|senate|vote|board|counc|capitol|voting|forum|pre-ele|advocacy')
sports <- c('btn|mnu|fleck|coach|sparks|sports|football|basketball|twins|bowl|game|hockey|golf|soccer|baseball|volleyball|halftime|timber|wild|rockies|gophers|lynx|tickets|homecoming|ncaa|frozen|athletics|bball|gymnastics|giants|big ten')
regions <- c('swfl|france|japan|phoenix|beijing|vietnam|milwaukee|korea|new jersey|asia|florida|arizona|california|washington|seattle|india|vietnam|colorado|atlanta|internation|china|london|nyc|chicago|new mexico|houston')
learning <- c('minnecollege|minne-|mcan|epn|career|tech|mentor|librar|talk|webinars|web|learning|coursera|lecture|speaker')
solicit <- c('gtmd|deal|opt|new grad|redeem|give|gift|appeal|donor|solicit|membership|survey|at the u|spring|fall|promo|offer|fundraiser|renew')
mass_updates <- c('aaw|system|news|angle|mem|non-|nonmem|announce|report|email|update|birthday')
social <- c('bts|dta|gala|brunch| at | in |suncoast|interest|date|gradfest|engage|resched|postpo|farewell|remind|volunteer|valleyfair|invite|holiday|connect|summit|leader|potluc|picnic|join|night|reunion|social|happy hour|mixer|networking|event|network|brew|wine|tour|travel|movie|breakfast|lunch|dinner|meet|alumni|skiumania|service|trip|gathering|awards|travel|celebration|party|ceremony|bike|ski-u')


emails_distinct <- emails_distinct[, broad_cat := 
    ifelse(grepl(paste(clothes, collapse='|'), DESC_PATH), 'Solicitations', 
    ifelse(grepl(paste(laws, collapse='|'), DESC_PATH), 'Legislature', 
    ifelse(grepl(paste(sports, collapse='|'), DESC_PATH), 'Sports', 
    ifelse(grepl(paste(learning, collapse='|'), DESC_PATH), 'Learning', 
    ifelse(grepl(paste(solicit, collapse='|'), DESC_PATH), 'Solicitations', 
    ifelse(grepl(paste(mass_updates, collapse='|'), DESC_PATH), 'Mass Updates', 
    ifelse(grepl(paste(social, collapse='|'), DESC_PATH), 'Social', 'Other')))))))]

emails_distinct <- emails_distinct[, !"DESC_PATH"]

emails_revised <- emails_revised[, !c("DESC_PATH", "COLUMN1")]
memory.limit(size=56000)
rm(emails)
emails_revised <- merge.data.table(emails_revised, emails_distinct, by = 'CODE_PATH')

emails_revised <- emails_revised %>% select(ID_DEMO.x, DATE_CONTACT.x, CODE_PATH, CODE_OUTCOME.x, YEAR_FISCAL.x, broad_cat) %>% 
  rename(ID_DEMO = ID_DEMO.x, 
         DATE_CONTACT = DATE_CONTACT.x, 
         CODE_OUTCOME = CODE_OUTCOME.x,
         YEAR_FISCAL = YEAR_FISCAL.x)

fwrite(emails_revised, "emails_cleaned.csv",row.names = FALSE)

