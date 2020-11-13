#set working directory
setwd('D:/Group Folder/SEQUENTIAL FILES')

## Libraries used

library(tidyverse)
library(flexclust)
library(lubridate)

## Variables used

# current fiscal year
current_fy = ifelse( month(Sys.Date()) > 6, year(Sys.Date()) + 1, 
                     year(Sys.Date())) 


# Running Clusters --------------------------------------------------------

cluster_df <- read_csv('3 Data Generated Files/cluster_prep.csv')

members <- cluster_df %>% filter(MEMBERSHIP_STATUS_CODE == "C")
non_members <- cluster_df %>% filter(MEMBERSHIP_STATUS_CODE != "C")


members[, 2:6] %>% head()


SSE_curve <- c()
for (n in 1:10) {
  kcluster <- kmeans(members[, 2:6], n)
  print(kcluster$withinss)
  sse <- sum(kcluster$withinss)
  SSE_curve[n] <- sse
}
plot(1:10, SSE_curve, type="b", xlab="Number of Clusters", ylab="SSE")




set.seed(1)
cl1 <- kcca(members[, 2:6], k=5, kccaFamily('kmeans'))

cluster <- predict(cl1, save.data=T)



members['cluster'] = cluster

cluster <- predict(cl1, newdata = non_members[, 2:6], save.data=T)
non_members['cluster'] = cluster

cluster_df <- rbind(members, non_members)


cluster_summary <- 
  cluster_df %>% 
  group_by(cluster) %>% 
  summarize_all(list(mean)) %>% select(cluster, learning, legis, social, sports, networking) %>% 
  mutate(Description = 
       ifelse(learning > .2, "Learning",
       ifelse(legis > .2, "Legis",
       ifelse(social > .2, "Social",
       ifelse(sports > .2, "Sports",
       ifelse(networking > .2, "Networking", "Baseline")))))) %>%
  select(cluster, Description)



cluster_df <- 
  cluster_df %>% 
  inner_join(cluster_summary, by = 'cluster')




# AS OF RIGHT NOW
# 1 - Networking
# 2 - Baseline
# 3 - Learning
# 4 - Social
# 5 - Sports



write.csv(cluster_df, '3 Data Generated Files/per_person_clusters.csv', row.names = F)
