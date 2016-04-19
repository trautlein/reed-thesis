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


senate_start_cong <- 66 # starting congress I want
# strips out 3rd party, before cong 60, make party variable D or R 
senate_data <- read.csv("data/SenateData.csv") %>% 
  select(cong:x2, role) %>%
  mutate(leader = ifelse(role == "", "N", "Y")) %>%
  dplyr::filter(party == 100 | party == 200) %>%
  dplyr::filter(state != "USA") %>% # remove presidents
  mutate(party2 = as.factor(ifelse(party == 100, "D","R"))) %>%
  select(-party) %>%
  rename(party = party2)


senate_party_means_table <- senate_data %>% 
  group_by(cong, party) %>%
  summarise(mean_ideo = mean(ideo))

senate_party_means_wide <- senate_party_means_table %>%
  spread(key = party, value = mean_ideo)


senate_party_means_wide$D_del <- vector(mode = "numeric", length = nrow(senate_party_means_wide))
senate_party_means_wide$R_del <- vector(mode = "numeric", length = nrow(senate_party_means_wide))
senate_party_means_wide$D_del[1] <- NA
senate_party_means_wide$R_del[1] <- NA
for(i in 2:nrow(senate_party_means_wide)){
  senate_party_means_wide$D_del[i] <- senate_party_means_wide$D[i] - senate_party_means_wide$D[i - 1]
  senate_party_means_wide$R_del[i] <- senate_party_means_wide$R[i] - senate_party_means_wide$R[i - 1]
}


senate_party_means_dif <- senate_party_means_wide %>%
  select(cong, D_del, R_del) %>%
  rename(D = D_del, R = R_del) %>%
  gather(key = party, value = party_del, D:R)


senate_merged <- senate_data %>% inner_join(senate_party_means_table, by = c("cong", "party")) %>%
  inner_join(senate_party_means_dif, by = c("cong", "party"))
senate_merged <- senate_merged %>%
  mutate(last_cong = ifelse(cong > senate_start_cong, cong - 1, NA))
senate_merged2 <- senate_merged %>% left_join(senate_party_means_table, by = c("last_cong" = "cong", "party")) %>%
  rename(cong_ideo_mean = mean_ideo.x, last_cong_ideo_mean = mean_ideo.y) %>%
  select(-last_cong)

senate_merged3 <- senate_merged2 %>%
  mutate(ideo_dif = cong_ideo_mean - ideo) %>%
  mutate(last_ideo = NA)




for (j in 1:nrow(senate_merged3)) { ###### TAKES A WHILE TO RUN! ######
  senate_merged3[j,]$last_ideo <- last_ideo_lookup(senate_merged3, 
                                                   senate_merged3[j,]$cong - 1, senate_merged3[j,]$icpsr)
}


senate_merged_D <- dplyr::filter(senate_merged3, party == "D")
senate_merged_R <- dplyr::filter(senate_merged3, party == "R")

senate_lmD <- lm(ideo ~ 
            cong_ideo_mean + # party mean
            last_cong_ideo_mean + # last party mean
            last_ideo + # last ideology score
            leader, # are they a leader
          data = senate_merged_D)

summary(senate_lmD)

senate_lmR <- lm(ideo ~ 
            cong_ideo_mean + # party mean
            last_cong_ideo_mean + # last party mean
            last_ideo + # last ideology score
            leader, # are they a leader
          data = senate_merged_R)

summary(senate_lmR)
