# Preparing data for matching using MALTS
# People who got referred (irrespective of conversion) will be matched with others in the population.
library(data.table)
library(ggplot2)


setwd('G:\\My Drive\\Research\\UMAA\\Data\\actual_exp')
# people who got referred
referred <- fread("life_referral_match.csv")

referred <- referred[, .(ID_DEMO_REFERRAL)][, ID_DEMO := ID_DEMO_REFERRAL][, ID_DEMO_REFERRAL:=NULL]



setwd('G:\\My Drive\\Research\\UMAA\\Data\\main_data\\ML files')

covar <- fread("all_data_for_final.csv")


# those who converted to memberships will be equal to 1
referred <- referred[!is.na(ID_DEMO)][, outcome:=1]

# merging that to the main df to add the outcome variable
main_df <- merge.data.table(covar, referred, all.x=TRUE)

# rest of the ones will be classified as 0 outcome as they did not convert
main_df <- main_df[is.na(outcome), outcome:=0]


setwd('G:\\My Drive\\Research\\UMAA\\Data\\Matching')

fwrite(main_df, "input_malts.csv", row.names = FALSE)
