library(data.table)


setwd('G:\\My Drive\\Research\\UMAA\\Code\\ml_model')
exclusion_list <- fread("exclusion_list.csv")


setwd('G:\\My Drive\\Research\\UMAA\\Data\\actual_exp')
ref <- fread("life_referral_match.csv")


ref <- ref[, .(ID_DEMO_REFERRAL)][, ID_DEMO := ID_DEMO_REFERRAL][, ID_DEMO_REFERRAL:=NULL]


merge_tbl <- merge.data.table(ref, exclusion_list, all=FALSE)



# we set 0 as belonging to exclusion list and 1 as belonging to referred group
set.seed(52)

flip <- sample(c(1,0), 5, replace = TRUE)

merge_tbl[, ref_group := flip]


# see where in ML group do the referred people fall.

setwd('G:\\My Drive\\Research\\UMAA\\Code\\ml_model')
ml_data <- fread("valid_predict_proba.csv")
id <- ml_data[ID_DEMO %in% merge_tbl$ID_DEMO]
