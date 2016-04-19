library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(readr)
library(DT)
options(scipen = 10, digits = 5)



##### FUNCTIONS ######

lookup <- function(df, val1, val2, col = 3){
  df[df[1] == val1 & df[2] == val2, col][1]
}

last_ideo_lookup <- function(df, cong, icpsr, col = 6){
  df[df[1] == cong & df[2] == icpsr, col][1]
}

#####################


house_cong_start <- 56 # starting congress I want

house_data <- read.csv("data/HouseData.csv") %>%
  select(cong:x2) %>%
  mutate(leader = ifelse(role == "", "N", "Y")) %>%
  dplyr::filter((party == 100 | party == 200) & cong >= house_cong_start) %>%
  dplyr::filter(state != "USA") %>% # remove presidents
  mutate(party2 = as.factor(ifelse(party == 100, "D","R"))) %>%
  select(-party) %>%
  rename(party = party2) %>%
  mutate(last_ideo = NA)



house_party_means_table <- house_data %>% 
  group_by(cong, party) %>%
  summarise(mean_ideo = mean(ideo))



house_party_means_wide <- house_party_means_table %>%
  spread(key = party, value = mean_ideo)



house_party_means_wide$D_del <- vector(mode = "numeric", length = nrow(house_party_means_wide))
house_party_means_wide$R_del <- vector(mode = "numeric", length = nrow(house_party_means_wide))
house_party_means_wide$D_del[1] <- NA
house_party_means_wide$R_del[1] <- NA
for(i in 2:nrow(house_party_means_wide)){
  house_party_means_wide$D_del[i] <- house_party_means_wide$D[i] - house_party_means_wide$D[i - 1]
  house_party_means_wide$R_del[i] <- house_party_means_wide$R[i] - house_party_means_wide$R[i - 1]
}


house_party_means_dif <- house_party_means_wide %>%
  select(cong, D_del, R_del) %>%
  rename(D = D_del, R = R_del) %>%
  gather(key = party, value = party_del, D:R)



house_merged <- house_data %>% inner_join(house_party_means_table, by = c("cong", "party")) %>%
  inner_join(house_party_means_dif, by = c("cong", "party"))
house_merged <- house_merged %>%
  mutate(last_cong = ifelse(cong > house_cong_start, cong - 1, NA))
house_merged2 <- house_merged %>% left_join(house_party_means_table, by = c("last_cong" = "cong", "party")) %>%
  rename(cong_ideo_mean = mean_ideo.x, last_cong_ideo_mean = mean_ideo.y) %>%
  select(-last_cong)

house_merged3 <- house_merged2 %>%
  mutate(ideo_dif = cong_ideo_mean - ideo) %>%
  mutate(last_ideo = NA)




for (j in 1:nrow(house_merged3)) {
  house_merged3[j,]$last_ideo <- lookup(house_merged3, house_merged3[j,]$cong - 1, house_merged3[j,]$icpsr, 6)
}



house_merged_D <- dplyr::filter(house_merged3, party == "D")
house_merged_R <- dplyr::filter(house_merged3, party == "R")



house_lmD <- lm(ideo ~ 
            cong_ideo_mean + # party mean
            last_cong_ideo_mean + # last party mean
            last_ideo + # last ideology score
            leader, # are they a leader
          data = house_merged_D)

summary(house_lmD)

house_lmR <- lm(ideo ~ 
            cong_ideo_mean + # party mean
            last_cong_ideo_mean + # last party mean
            last_ideo + # last ideology score
            leader, # are they a leader
          data = house_merged_R)

summary(house_lmR)

