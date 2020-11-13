setwd("D:/Group Folder/SEQUENTIAL FILES/")

## Load the below libraries
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)


########## Data transformation for Insights

# Load cleaned membership file

mem_cleaned <- read.csv("2 Data Cleaned Files/membership_cleaned.csv")

# Creating a new column to check if a membership purchase is complementary or Life or Annual
mem_cleaned <- mem_cleaned %>% 
               mutate(mem_type = ifelse( str_detect(str_to_lower(MEMBERSHIP_LEVEL), "comp"), "Comp",
                                      ifelse( str_detect(str_to_lower(MEMBERSHIP_LEVEL), "life"),"Life"
                                              ,"Annual")))

# Get Alumni IDs who tried complementary membership
comp_ids <- mem_cleaned %>% 
            filter(mem_type == "Comp") %>% 
            select(ID_DEMO)

comp_ids <- unique(comp_ids$ID_DEMO)

# Identify Alumni who tried Annual membership atleast once
annual_ids_atleast_once <- mem_cleaned %>% 
                            filter(mem_type == "Annual") %>% 
                            select(ID_DEMO)

annual_ids_atleast_once <- unique(annual_ids_atleast_once$ID_DEMO)


# Load Individual Info File
ind_info <- read.csv("1 Data Initial Files/individual_info.csv")

# Idenfity current Life members
current_life <- ind_info %>% filter( (MEMBERSHIP_TYPE_CODE == "L") &
                                      MEMBERSHIP_STATUS_CODE == "C")

current_life <- unique(current_life$ID_DEMO)

# Idenfity current Annual members
current_annual <- ind_info %>% filter( (MEMBERSHIP_TYPE_CODE == "A") &
                                        MEMBERSHIP_STATUS_CODE == "C")
current_annual <- unique(current_annual$ID_DEMO)

############### Insights (Alumni groups based on membership status)

# Total alumni = 617372
length(ind_info$ID_DEMO)

# Alumni who tried Annual membership atleast once = 152552
length(annual_ids_atleast_once)

# Alumni who are Life members but did not try Annual membership atleast once = 7420
length(setdiff(current_life,annual_ids_atleast_once ))

# Alumni who havenot taken Life or Annual membership = 457400
# (complementary members who did not buy any membership are counted as Non members) 
617372 - 152552 - 7420

# Alumni who tried Annual membership atleast once and became Life member = 12894
length(intersect(current_life,annual_ids_atleast_once ))

# Alumni who tried Annual membership atleast once and current Annual member = 13585
length(intersect(current_annual,annual_ids_atleast_once ))

# Alumni who tried Annual membership atleast once but discontinued membership = 126073
152552 - 13585 - 12894


############### Insights (Life members past behaviour)

#total Life members = 20314
length(current_life)

# Alumni who didnot try Annual membership but bougth Life membership = 7420
# (complementary members who did not buy any membership are counted as Non members)
length(setdiff(current_life,annual_ids_atleast_once ))

# Alumni who tried Annual membership atleast once and became Life member = 12894
length(intersect(current_life,annual_ids_atleast_once ))
