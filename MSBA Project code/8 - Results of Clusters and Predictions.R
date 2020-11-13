setwd("D:/Group Folder/SEQUENTIAL FILES")

library(tidyverse)

clusters <- read_csv('3 Data Generated Files/per_person_clusters.csv')

# Lifetime and Churn ------------------------------------------------------------------


life_churn <- 
  read_csv('5 Predictive Files/Prediction_Member_Retention_2020.csv') %>%
  select(ID_DEMO, Probability_Life, Probability_Churn, MEMBERSHIP_TYPE_CODE, MEMBERSHIP_STATUS_CODE) %>% 
  inner_join(clusters, by = c('ID_DEMO', 'MEMBERSHIP_TYPE_CODE', 'MEMBERSHIP_STATUS_CODE'))


write.csv(life_churn, '3 Data Generated Files/life_churn_clusters_predictions.csv', row.names=F)



# Non-member --------------------------------------------------------------

non_member <- 
  read_csv('5 Predictive Files/Prediction_NewMember_Acquisition_2020.csv') %>% 
  select(ID_DEMO, Probability_NewMember, MEMBERSHIP_TYPE_CODE, MEMBERSHIP_STATUS_CODE) %>% 
  inner_join(clusters, by = c('ID_DEMO', 'MEMBERSHIP_TYPE_CODE', 'MEMBERSHIP_STATUS_CODE'))


write.csv(non_member, '3 Data Generated Files/non_clusters_predictions.csv', row.names=F)

