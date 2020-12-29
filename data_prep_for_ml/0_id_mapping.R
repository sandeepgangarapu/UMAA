## set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\ids')

df <- fread("ids.csv")
df <- df[complete.cases(df),]
df <- unique(df)

fwrite(df, "unique_id_mapped.csv")
