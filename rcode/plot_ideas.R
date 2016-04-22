library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(readr)
library(DT)
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

p_vals <- NULL
get_p_val <- function(data, start_cong, end_cong = 113) {
  working_lm <- cong_lm(data, start_cong, end_cong)
  p_val <- unname(summary(working_lm)$coefficients[,4][5])
  return(p_val)
}

get_p_vals <- function(data, start_cong, end_cong = 113) {
  p_vals$cong <- NA
  p_vals$p_val <- NA
  
  for(i in start_cong:end_cong) {
    p_vals[i - start_cong - 1] <- get_p_val(data, i, end_cong)
  }
  
  p_vals_df <- data.frame(Reduce(rbind, p_vals))
  
  return(p_vals_df)
}
##### END FUNCTIONS ####################################

# from 1921 to 2015
sum_cong_lm(senate_D, 67, 113)
sum_cong_lm(senate_R, 67, 113)
sum_cong_lm(house_D, 67, 113)
sum_cong_lm(house_R, 67, 113)


sum_cong_lm(senate_D, 85, 101) 


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


