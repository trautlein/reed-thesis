library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(knitr)
options(scipen = 10, digits = 5)


#### LOAD CODE ####
senate_D <- read.csv("~/thesis/data/Senate_D.csv", header = TRUE)
senate_R <- read.csv("~/thesis/data/Senate_R.csv", header = TRUE)

house_D <- read.csv("~/thesis/data/House_D.csv", header = TRUE)
house_R <- read.csv("~/thesis/data/House_R.csv", header = TRUE)

senate_means <- read.csv("~/thesis/data/Senate_Means.csv", header = TRUE)
house_means <- read.csv("~/thesis/data/House_Means.csv", header = TRUE)
###################


##### FUNCTIONS #########
cong_lm <- function(data, start_cong, end_cong){
  
  working_data <- dplyr::filter(data, 
                                cong >= start_cong,
                                cong <= end_cong)
  
  
  lm <- lm(ideo ~ 
             cong_ideo_mean + # party mean
             last_cong_ideo_mean + # last party mean
             last_ideo + # last ideology score
             leader, # are they a leader
           data = working_data)
  return(lm)
}

sum_cong_lm <- function(data, start_cong = 67, end_cong = 113){
  summary(cong_lm(data, start_cong, end_cong))
}


one_congress <- function(chamber, cong_session, RorD = NA) {
  working <- dplyr::filter(chamber, cong == cong_session)
  
  ifelse(is.na(RorD),
         return(working),
         return(dplyr::filter(working, party == RorD))
         )
}

##### END FUNCTIONS ####################################

# join parties together to create complete datasets
senate <- suppressWarnings(dplyr::bind_rows(senate_D, senate_R) %>% arrange(cong, state, name))
house <- suppressWarnings(dplyr::bind_rows(house_D, house_R) %>% arrange(cong, state, name))

senate_means_short <- tidyr::spread(senate_means, party, mean_ideo)

house_means_short <- tidyr::spread(house_means, party, mean_ideo)

party_colors <- c("R" = "red","D" = "blue")




