#set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\main_data')

## Libraries used

library(lubridate)
library(data.table)
library(stringr)



# 2 Data Cleaned Files

indy_info_main <- fread('individual_info_cleaned.csv')
membership_main <- fread('membership_cleaned.csv')


spouse_id <- copy(indy_info_main)[!is.na(ID_SPOUSE), .(ID_SPOUSE)]

rec_in_ind <- spouse_id[ID_SPOUSE %in% main_id]
rec_not_in_ind <- spouse_id[!ID_SPOUSE %in% main_id]

ind_mem <- rec_in_ind[ID_SPOUSE %in% mem_id]
ind_no_mem <- rec_in_ind[!ID_SPOUSE %in% mem_id]
no_ind_mem <- rec_not_in_ind[ID_SPOUSE %in% mem_id]
no_ind_no_mem <- rec_not_in_ind[!ID_SPOUSE %in% mem_id]
