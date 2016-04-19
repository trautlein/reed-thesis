library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(readr)
library(DT)
options(scipen = 10, digits = 5)


house_cong_start <- 56 # starting congress I want

house_start <- read.csv("data/HouseData.csv") %>%
  select(cong:x2) %>%
  dplyr::filter((party == 100 | party == 200) & cong >= house_cong_start) %>%
  dplyr::filter(state != "USA") %>% # remove presidents
  mutate(party2 = as.factor(ifelse(party == 100, "D","R"))) %>%
  select(-party) %>%
  rename(party = party2) %>%
  mutate(last_ideo = NA)



house_party_means_table <- house_start %>% 
  group_by(cong, party) 
  summarise(mean_ideo = mean(ideo))


start_cong_house <- 56 # starting congress I want
house_data <- read.csv("data/HouseData.csv") %>%
  select(cong:x2, role) %>%
  mutate(leader = ifelse(role == "", "N", "Y")) %>%
  dplyr::filter(party == 100 | party == 200) %>%
  dplyr::filter(state != "USA") %>% # remove presidents
  mutate(party2 = as.factor(ifelse(party == 100, "D","R"))) %>%
  select(-party) %>%
  rename(party = party2)



house_party_means_table <- house_data %>% 
  group_by(cong, party) %>%
  summarise(mean_ideo = mean(ideo))


house_party_means_wide <- house_party_means_table %>%
  spread(key = party, value = mean_ideo)



house_party_means_wide$D_del <- vector(mode = "numeric", length = nrow(house_party_means_wide))
house_party_means_wide$R_del <- vector(mode = "numeric", length = nrow(house_party_means_wide))
house_party_means_wide$D_del[1] <- NA
house_party_means_wide$R_del[1] <- NA
for(i in 2:nrow(party_means_wide)){
  house_party_means_wide$D_del[i] <- house_party_means_wide$D[i] - house_party_means_wide$D[i - 1]
  house_party_means_wide$R_del[i] <- house_party_means_wide$R[i] - house_party_means_wide$R[i - 1]
}


house_party_means_dif <- house_party_means_wide %>%
  select(cong, D_del, R_del) %>%
  rename(D = D_del, R = R_del) %>%
  gather(key = party, value = party_del, D:R)


