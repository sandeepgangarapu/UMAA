#set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\main_data')

## Libraries used

library(lubridate)
library(data.table)
indy_info_main <- fread('individual_info_cleaned.csv')
membership <- fread('membership_cleaned.csv')
current_fy = 2021
membership <- membership[FISCAL_YEAR < current_fy-2][, rnk:=frank(-FISCAL_YEAR), .(ID_DEMO)][rnk==1]
membership <- membership[,.(ID_DEMO, FISCAL_YEAR, MEMBERSHIP_LEVEL)]
training <- fread('ML files/new_member_training.csv')

# Data prep for demographic data

table(indy_info_main$MARITAL_STATUS)
# Looks like married and unmarried are the dominant. 
# others can be clubbed into

individual <- copy(indy_info_main)[,`:=`(MARITAL_STATUS = ifelse(MARITAL_STATUS== 'M' | MARITAL_STATUS=='U', MARITAL_STATUS, 'O'),
                                         GENDER = ifelse(GENDER=='F', 1, 0),
                                         IN_TC_METRO_AREA = ifelse(IN_TC_METRO_AREA=='Y', 1, 0),
                                         IS_CURRENT_TC_EMPLOYEE = ifelse(IS_CURRENT_TC_EMPLOYEE=='N', 0, 1),
                                         ATHLETIC_INTEREST = ifelse(ATHLETIC_INTEREST=='Y', 1, 0),
                                         TRAVEL_INTEREST = ifelse(TRAVEL_INTEREST=='Y', 1, 0),
                                         AFFINITY_NETWORK_INTEREST = ifelse(is.na(AFFINITY_NETWORK_INTEREST), 0, 1),
                                         WEB_TOPIC_OPT_INS = ifelse(is.na(WEB_TOPIC_OPT_INS), 0, 1))]
                                         
individual <- individual[,.(ID_DEMO, MARITAL_STATUS, GENDER, AGE,
                            IN_TC_METRO_AREA, IS_CURRENT_TC_EMPLOYEE,
                            ATHLETIC_INTEREST, TRAVEL_INTEREST, AFFINITY_NETWORK_INTEREST,
                            WEB_TOPIC_OPT_INS)]                             
                             

final_df <- merge.data.table(individual, training, by="ID_DEMO")
final_df <- final_df[membership_TwoYearsAgo=="non_member"]

final_df[, target := ifelse(membership_LastYear!="non_member", 1,0)]

final_df <- final_df[, `:=`(membership_LastYear=NULL, membership_TwoYearsAgo=NULL)]

# Now we see if they have even been a member before
final_df <- merge.data.table(final_df, membership, by="ID_DEMO", all.x=TRUE)
final_df <- final_df[,member_before:= ifelse(is.na(MEMBERSHIP_LEVEL), 0, 1)][, years_before:=ifelse(is.na(FISCAL_YEAR), 100, current_fy-2-FISCAL_YEAR)]
final_df <- final_df[, `:=`(FISCAL_YEAR=NULL, MEMBERSHIP_LEVEL=NULL, annual_member=NULL, life_member=NULL, non_member=NULL)]


fwrite(final_df, "ML files/data_for_model.csv", row.names = FALSE)


                         