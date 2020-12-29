#set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Code\\ml_model')

## Libraries used

library(lubridate)
library(data.table)
require(Hmisc)
model_data <- fread("data_for_model.csv")
all_data <- fread("all_data_for_final.csv")


## set working directory
setwd('G:\\My Drive\\Research\\UMAA\\Data\\ids')

ids <- fread("unique_id_mapped.csv")
ids <- unique(ids)
# Get income data

# 

setwd("G:\\My Drive\\Research\\UMAA\\Data\\CAL Data\\Initial Data Files Shared with CAL Team")

income_data <- fread("individual_info.csv")

income_main <- income_data[, .(ID_DEMO, HOUSEHOLD_INCOME)]
test <- copy(income_main)[, .(cnt = .N),.(ID_DEMO)]

income_spouse <- income_data[, .(ID_SPOUSE, HOUSEHOLD_INCOME)][!ID_SPOUSE=='']
income_spouse <- income_spouse[,rnk := rank(-HOUSEHOLD_INCOME),.(ID_SPOUSE)][rnk==1][,rnk:=NULL]

income_final <- rbind(income_main[, .(ID_DEMO, HOUSEHOLD_INCOME)], income_spouse[, .(ID_SPOUSE, HOUSEHOLD_INCOME)], use.names=FALSE)
income_final <- income_final[!is.na(ID_DEMO)][!ID_DEMO=='']
income_final <- income_final[,rnk := rank(-HOUSEHOLD_INCOME),.(ID_DEMO)][rnk==1][,rnk:=NULL]
test <- copy(income_final)[, .(cnt = .N),.(ID_DEMO)]

income_with_new_id <- merge.data.table(ids, income_final, by.x = "ENCRYPT_ID_DEMO", by.y="ID_DEMO", all.x = TRUE) 
income_with_new_id <- income_with_new_id[, ENCRYPT_ID_DEMO:=NULL]


d <- density(log10(income_with_new_id$HOUSEHOLD_INCOME), na.rm = TRUE)
plot(d, main='')

income_with_new_id[, HOUSEHOLD_INCOME := log10(HOUSEHOLD_INCOME)]
income_with_new_id[, income_bucket := ifelse(HOUSEHOLD_INCOME <= 3.7, 1,
                                             ifelse(HOUSEHOLD_INCOME > 3.7 & HOUSEHOLD_INCOME <=4.2, 2,
                                                    ifelse(HOUSEHOLD_INCOME > 4.2 & HOUSEHOLD_INCOME <=4.7, 3,
                                                           ifelse(HOUSEHOLD_INCOME > 4.7 & HOUSEHOLD_INCOME <=5.2, 4,
                                                                  ifelse(HOUSEHOLD_INCOME > 5.2 & HOUSEHOLD_INCOME < 5.6, 5, 6)))))]

income_with_new_id[, income_bucket := ifelse(is.na(HOUSEHOLD_INCOME), 7, income_bucket)]
# we convert the household income in to log scale and divide into 7 buckets. 1 bucket for na, 1 for less than 3.7 as per the density graph. One for greater than 5.7/ others get the rest 3 equally



model_data_new <- merge.data.table(model_data, income_with_new_id, by.x = "ID_DEMO", by.y="ID_DEMO", all.x = TRUE) 
model_data_new <- model_data_new[, income_bucket:=ifelse(is.na(income_bucket), 7, income_bucket)]



# kmeans clustering
inc_vec <- model_data_new$HOUSEHOLD_INCOME[!is.na(model_data_new$HOUSEHOLD_INCOME)]

wss <- numeric(0)
for (i in c(1:10)){
 a <- kmeans(inc_vec, i, iter.max = 10, nstart=5)
print(a$tot.withinss)
wss <- c(wss, a$tot.withinss)
}

plot(c(1:10), wss)

# looks like 5 clusters are optimal 
a <- kmeans(inc_vec, 5, iter.max = 10, nstart=5)

# building k means using this package so that you can predict the cluster in new datapoints
#install.packages("flexclust")
library(flexclust)
cl1 = kcca(inc_vec, k=5, kccaFamily("kmeans"))
income_clust_train <- predict(cl1, model_data_new$HOUSEHOLD_INCOME)

# replacing NA with cluster no 0

income_clust_train[is.na(income_clust_train)] <- 0
model_data_new[, income_bucket:= income_clust_train]
# 
# 
# # GMM clustering
# library(ClusterR)
# #install.packages("ClusterR")
# 
# # replace NA income with 0
# model_data_new$HOUSEHOLD_INCOME[is.na(model_data_new$HOUSEHOLD_INCOME)] <- 0
# 
# 
# opt_gmm = Optimal_Clusters_GMM(as.matrix(inc_vec), max_clusters = 10, criterion = "BIC", 
#                                dist_mode = "eucl_dist", seed_mode = "random_subset",
#                                km_iter = 10, em_iter = 10, var_floor = 1e-10,
#                                plot_data = T)
# # as per this, 7 seems like a good cluster number
# 
# gmm = GMM(as.matrix(inc_vec), 7, dist_mode = "eucl_dist", seed_mode = "random_subset", km_iter = 10,
#           em_iter = 10, verbose = F)     
# 
# income_clust_train <- predict_GMM(as.matrix(model_data_new$HOUSEHOLD_INCOME), gmm$centroids, gmm$covariance_matrices, gmm$weights)
# model_data_new[, income_bucket:= income_clust_train$cluster_labels]


all_data_new <- merge.data.table(all_data, income_with_new_id, by.x = "ID_DEMO", by.y="ID_DEMO", all.x = TRUE) 
all_data_new <- all_data_new[, income_bucket:=ifelse(is.na(income_bucket), 7, income_bucket)]


# KNN clustering valid data
income_clust_valid <- predict(cl1, all_data_new$HOUSEHOLD_INCOME)
# replacing NA with cluster no 0
income_clust_valid[is.na(income_clust_valid)] <- 0
all_data_new[, income_bucket:= income_clust_valid]



# # GMM clustering on valid data
# all_data_new$HOUSEHOLD_INCOME[is.na(all_data_new$HOUSEHOLD_INCOME)] <- 0
# income_clust_valid <- predict_GMM(as.matrix(all_data_new$HOUSEHOLD_INCOME), gmm$centroids, gmm$covariance_matrices, gmm$weights)
# all_data_new[, income_bucket:= income_clust_valid$cluster_labels]
# 

setwd('G:\\My Drive\\Research\\UMAA\\Code\\ml_model')

fwrite(all_data_new, "all_data_for_final_income_knn.csv", row.names = FALSE)
fwrite(model_data_new, "data_for_model_income_knn.csv", row.names = FALSE)
