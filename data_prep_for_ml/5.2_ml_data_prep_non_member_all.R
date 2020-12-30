#set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\fresh_data')

## Libraries used

library(lubridate)
library(data.table)
indy_info_main <- fread('individual_info_cleaned.csv')
training <- fread('ML files/new_member_all_data.csv')

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

fwrite(final_df, "ML files/all_data_for_final.csv", row.names = FALSE)


                         