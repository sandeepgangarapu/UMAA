library(data.table)
setwd('G:\\My Drive\\Research\\UMAA\\Code\\ml_model')
exclusion_list <- fread("exclusion_list.csv")
ml_data <- fread("valid_predict_proba.csv")
setwd('G:\\My Drive\\Research\\UMAA\\Data\\Pilot')
pilot_exclusion <- fread("pilot_exclusion_ids.csv")
ml_data <- ml_data[!ID_DEMO %in% pilot_exclusion$ID_DEMO][order(-prob)][, .(ID_DEMO, prob)][, rnk:=1:.N]
ml_40000 <- ml_data[rnk<=40000]


main_ml <- exclusion_list[group=='ml_group']
silent_ml <- ml_40000[!ID_DEMO %in% main_ml$ID_DEMO][, group:='silent_ml'][,.(ID_DEMO, group)]

control_group <- exclusion_list[group=='control_group']


all_silent <- rbind(silent_ml, control_group)

setwd('G:\\My Drive\\Research\\UMAA\\Code\\ml_model')


fwrite(all_silent, "all_silent.csv")
