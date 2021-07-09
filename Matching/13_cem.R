library(data.table)
setwd('G:\\My Drive\\Research\\UMAA\\Data\\actual_exp')
ref <- fread("life_referral_match.csv")


ref <- ref[, .(ID_DEMO_REFERRAL)][, ID_DEMO := ID_DEMO_REFERRAL][, ID_DEMO_REFERRAL:=NULL][!is.na(ID_DEMO)]
ref <- ref[,trt:=1]
setwd('G:\\My Drive\\Research\\UMAA\\Code\\ml_model')
exclusion_list <- fread("exclusion_list.csv")
exclusion_list <- exclusion_list[,.(ID_DEMO)][, trt:=0]


all_data <- rbind(ref, exclusion_list)


setwd('G:\\My Drive\\Research\\UMAA\\Data\\main_data\\ML files')

covar <- fread("all_data_for_final.csv")

main_df <- merge.data.table(all_data, covar, all=FALSE)
all_groups <- c(800226038,  730377210,  730377209,  730253729,  730251549,  730102256,  700559994,  700130794,  700038025,  700028785,  700005721,  630171329,  630133336,  530313109,  530018570,  500101416,  500034747,  430245919,  430189943,  400447887,  400409667,  400199133,  400111426,  400004296,  300548754,  300433247,  300119049,  300111472,  300087191,  230093692,  230003647,  200536689,  200125028,  200038119,  140677387,  140429471,  140414252,  140214352,  140178519,  140167347,  140013280)
referred <- data.table(ID_DEMO = all_groups)
referred <- unique(referred)

# those who converted to memberships will be equal to 1
referred <- referred[, outcome:=1 ]

# merging that to the main df to add the outcome variable
main_df <- merge.data.table(main_df, referred, all.x=TRUE)

# rest of the ones will be classified as 0 outcome as they did not convert
main_df <- main_df[is.na(outcome), outcome:=0]


library(cem)

table(main_df$trt)

vars <- c("UMN_event","UMN_member","UMN_donor",
          "UMN_volun","UMN_inform","UMN_loyalty","UMN_avg_Annual_score_5_years","Learning_emails",
          "Legislature_emails","Social_emails","Sports_emails","general_ctr_emails","Learning_events",
          "Legislature_events","Networking_events","Other_events","Social_events","Sports_events",
          "total_type_person_events","MARITAL_STATUS","GENDER","AGE","IN_TC_METRO_AREA",
          "IS_CURRENT_TC_EMPLOYEE","ATHLETIC_INTEREST","TRAVEL_INTEREST","AFFINITY_NETWORK_INTEREST","WEB_TOPIC_OPT_INS",
        
          "member_before","years_before")

top_vars <- c("AFFINITY_NETWORK_INTEREST", "AGE", "MARITAL_STATUS", "ATHLETIC_INTEREST", "IN_TC_METRO_AREA", "GENDER",
              "IS_CURRENT_TC_EMPLOYEE", "TRAVEL_INTEREST", "trt")

top_df <- main_df[,..top_vars]

imbalance(group=main_df$trt, data=main_df,  drop = c('ID_DEMO', 'trt'))
mat <- cem(treatment = "trt", data = main_df, drop = c('ID_DEMO', 'trt'),keep.all=TRUE)
mat
est <- att(mat, outcome ~ trt, data=main_df, model="linear")
est

imbalance(group=top_df$trt, data=top_df,  drop = c('ID_DEMO', 'trt'))
mat <- cem(treatment = "trt", data = top_df, drop = c('ID_DEMO', 'trt'),keep.all=TRUE)
mat

