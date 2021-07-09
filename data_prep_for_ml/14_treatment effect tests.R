library(data.table)

id_email <- c(230093692,  200536689,  200125028,  140677387,  300087191,  300119049,  300548754,  230003647,  500101416,  430189943,  140429471,  430245919,  400409667,  140167347,  400004296,  140414252,  500034747,  530313109,  630171329,  140013280,  730251549,  730253729,  730102256,  730377210,  730377209,  800226038,  700028785,  200038119,  300433247,  400111426,  400447887,  400199133,  630133336,  140214352,  700038025,  700005721,  140178519,  300111472,  530018570,  700559994,  700130794,  140924045,  100056590,  300092826,  700561126,  700473227,  830308533,  130230060,  140435438,  140707906,  430133078,  530009761,  140671604,  500422079,  140435481,  600003643,  700072158,  700411959,  140008891,  730167855,  730033464,  700076666,  140130637,  800521172,  140188001,  330149182,  140134448,  140923893,  200229735,  200072537,  200412334,  330162920,  140257792,  500034001,  500003042,  500135110,  140020243)
id_silent <- c(100549097, 600072197, 500435896, 600117039, 140040012, 730286188, 400461849, 140007582, 300069093, 830248230, 200099584, 140188001, 300561954, 400021890, 200461334, 300092826, 330318779, 500532629, 140638967, 730102256, 400042838, 100550812, 530189789, 600561937, 140152570, 500105549, 140020186, 330052623, 300020359)
setwd('G:\\My Drive\\Research\\UMAA\\Code\\ml_model')

exclusion_list <- fread("exclusion_list.csv")
silent_list <- fread("all_silent.csv")
silent_ml_list <- silent_list[group=='silent_ml']
all_groups <- rbind(exclusion_list, silent_ml_list)

emain_converts <- data.table(ID_DEMO = id_email)
emain_converts <- unique(emain_converts)
silent_converts <- data.table(ID_DEMO = id_silent)
silent_converts <- unique(silent_converts)

new_members <- rbind(emain_converts, silent_converts)
main_tbl <- merge.data.table(all_groups, new_members, all=FALSE)

table(all_groups$group)
table(main_tbl$group)


prop.test(c(59,3), c(19985, 4760), alternative = c("greater"))
prop.test(c(59,1), c(19985, 84), alternative = c("two.sided"))
prop.test(c(1,3), c(84, 4760), alternative = c("greater"))
prop.test(c(59, 32), c(19985, 20015), alternative = c("greater"))

test <- data.table(ID_DEMO = c(140178519,	700130794))
test_2 <- merge.data.table(exclusion_list, test, all=FALSE)


# this is for removing spouses of the referred people

all_groups <- c(800226038,  730377210,  730377209,  730253729,  730251549,  730102256,  700559994,  700130794,  700038025,  700028785,  700005721,  630171329,  630133336,  530313109,  530018570,  500101416,  500034747,  430245919,  430189943,  400447887,  400409667,  400199133,  400111426,  400004296,  300548754,  300433247,  300111472,  300087191,  230093692,  230003647,  200536689,  200125028,  140677387,  140429471,  140414252,  140214352,  140178519,  140013280)
referred <- data.table(ID_DEMO = all_groups)
referred <- unique(referred)

main_tbl <- merge.data.table(exclusion_list, referred, all=FALSE)

table(exclusion_list$group)
table(main_tbl$group)


prop.test(c(13,3), c(5000, 99), alternative = c("two.sided"))



prop.test(c(31,2), c(19985, 99), alternative = c("two.sided"))



