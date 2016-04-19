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



start_cong_house <- 56 # starting congress I want
h1 <- read.csv("data/HouseData.csv") %>%
  select(cong:x2, role)

h2 <- h1 %>% # strips out 3rd party, before cong 60, make party variable D or R instead of 100 or 200
  mutate(leader = ifelse(role == "", "N", "Y")) %>%
#  mutate(leader_now = ifelse(role != "W" | "L" | "WL" | "LN", "0", "1")) %>%
  dplyr::filter((party == 100 | party == 200) & cong >= start_cong_house) %>%
  dplyr::filter(state != "USA") %>% # remove presidents
  mutate(party2 = as.factor(ifelse(party == 100, "D","R"))) %>%
  select(-party) %>%
  rename(party = party2)



party_means_table <- h2 %>% 
  group_by(cong, party) %>%
  summarise(mean_ideo = mean(ideo))
