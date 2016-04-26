library(tidyr)
library(dplyr)
library(ggplot2)
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

# # from 1921 to 2015
# lm_senate_D <- sum_cong_lm(senate_D, 67, 113)
# lm_senate_R <- sum_cong_lm(senate_R, 67, 113)
# lm_house_D <- sum_cong_lm(house_D, 67, 113)
# lm_house_R <- sum_cong_lm(house_R, 67, 113)

# join parties together to create complete datasets
senate <- suppressWarnings(dplyr::bind_rows(senate_D, senate_R) %>% arrange(cong, state, name))
house <- suppressWarnings(dplyr::bind_rows(house_D, house_R) %>% arrange(cong, state, name))



# # create single years for senate
# senate_R_113 <- one_congress(senate, 113, "R")
# senate_D_113 <- one_congress(senate, 113, "D")
# senate_113 <- one_congress(senate, 113)
# 
# 
# house_R_113 <- one_congress(house, 113, "R")
# house_D_113 <- one_congress(house, 113, "D")
# house_113 <- one_congress(house, 113)
 


# plot of how mean_ideo by party has changed over congresses
# ggplot(data = senate_means, aes(x = cong, y = mean_ideo, color = party)) +
#   geom_line(size = 1) + geom_point(size = 3) +
#   scale_color_brewer(palette = "Set1") + ylim(c(-1, 1)) +
#   scale_color_manual(values=c("blue", "red")) + 
#   xlab("Congressional Session") + ylab("Mean Ideology (D-NOMINATE)") +
#   ggtitle("Mean Senate Ideological Scores Separated By Party")


# ggplot(data = house_means, aes(x = cong, y = mean_ideo, color = party)) +
#   geom_line(size = 1) + geom_point(size = 3) +
#   scale_color_brewer(palette = "Set1") + ylim(c(-1, 1)) +
#   scale_color_manual(values=c("blue", "red")) +
#   xlab("Congressional Session") + ylab("Mean Ideology (D-NOMINATE)") +
#   ggtitle("Mean House Ideological Scores Separated By Party")
# 


