#set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\fresh_data')

## Libraries used

library(lubridate)
library(data.table)
indy_new <- fread('individual_info_cleaned.csv')

#set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\CAL Data\\Initial Data Files Cleaned')
indy_old <- fread('individual_info_cleaned.csv')


new_records <- indy_new[!(ID_DEMO %in% indy_old$ID_DEMO), .(new=.N)]


# Conclusiion; As ID_DEMO is different, there is no way to reliale match people