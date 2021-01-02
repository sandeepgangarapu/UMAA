setwd('G:\\My Drive\\Research\\UMAA\\Code\\ml_model')
valid_pred <- fread("valid_predict_proba.csv")


all_data <- fread("all_data_for_final.csv")

setwd('G:\\My Drive\\Research\\UMAA\\Data\\main_data')
membership <- fread("membership_cleaned.csv")


b <- merge.data.table(all_data, membership, all=FALSE)
