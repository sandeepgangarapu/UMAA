library(data.table)
library(ggplot2)
setwd('G:\\My Drive\\Research\\UMAA\\Code\\ml_model')
ml_data <- fread("valid_predict_proba.csv")


all_groups <- c(230093692,  200536689,  200125028,  140677387,  300087191,  300119049,  300548754,  230003647,  500101416,  430189943,  140429471,  430245919,  400409667,  140167347,  400004296,  140414252,  500034747,  530313109,  630171329,  140013280,  730251549,  730253729,  730102256,  730377210,  730377209,  800226038,  700028785,  200038119,  300433247,  400111426,  400447887,  400199133,  630133336,  140214352,  700038025,  700005721,  140178519,  300111472,  530018570,  700559994,  700130794,  140924045,  100056590,  300092826,  700561126,  700473227,  830308533,  130230060,  140435438,  140707906,  430133078,  530009761,  140671604,  500422079,  140435481,  600003643,  700072158,  700411959,  140008891,  730167855,  730033464,  700076666,  140130637,  800521172,  140188001,  330149182,  140134448,  140923893,  200229735,  200072537,  200412334,  330162920,  140257792,  500034001,  500003042,  500135110,  140020243)


setwd('G:\\My Drive\\Research\\UMAA\\Code\\ml_model')
exclusion_list <- fread("exclusion_list.csv")
referred <- data.table(ID_DEMO = all_groups)
referred <- unique(referred)

merge_tbl <- merge.data.table(referred, ml_data, all=FALSE)

ml_convert <- merge_tbl[prob>0.5838644]
ml_group <- exclusion_list[group=='ml_group']
ml_group <- merge.data.table(ml_group, ml_data, all=FALSE)


ml_group <- ml_group[, rnd := ceiling(prob*10)]
ml_group_rnd <- ml_group[, .(prop = .N), .(factor(rnd))][, prop:=prop/19985]

ml_convert[, rnd := ceiling(prob*10)]
ml_conv_rnd <- ml_convert[, .(prop = .N), .(factor(rnd))][, prop:=prop/62]

ggplot(ml_data, aes(x=prob)) + geom_density() + geom_vline(data=merge_tbl,aes(xintercept=prob)) + geom_vline(xintercept = 0.5838644, color='red', linetype = "longdash")

ggplot(ml_data[prob>0.5838644,], aes(x=prob)) + geom_density() + geom_density(data=merge_tbl[prob>0.5838644,], aes(x=prob), color='red')

library(stats)
ks.test(ml_data$prob, merge_tbl$prob)
