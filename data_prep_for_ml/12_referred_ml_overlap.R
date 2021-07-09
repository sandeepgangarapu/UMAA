library(data.table)
library(ggplot2)
setwd('G:\\My Drive\\Research\\UMAA\\Code\\ml_model')
ml_data <- fread("valid_predict_proba.csv")

setwd('G:\\My Drive\\Research\\UMAA\\Data\\actual_exp')
ref <- fread("life_referral_match.csv")


ref <- ref[, .(ID_DEMO_REFERRAL)][, ID_DEMO := ID_DEMO_REFERRAL][, ID_DEMO_REFERRAL:=NULL]

merge_tbl <- merge.data.table(ref, ml_data, all=FALSE)

ggplot(ml_data, aes(x=prob)) + geom_density() + geom_vline(data=merge_tbl,aes(xintercept=prob)) + geom_vline(xintercept = 0.5838644, color='red', linetype = "longdash")

ggplot(ml_data, aes(x=prob)) + geom_density() + geom_density(data=merge_tbl, aes(x=prob))

library(stats)
ks.test(ml_data$prob, merge_tbl$prob)
