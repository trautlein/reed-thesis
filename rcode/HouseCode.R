library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(readr)
library(DT)
options(scipen = 10, digits = 5)


start_cong <- 82 # starting congress I want

d <- read.csv("data/HouseData.csv") %>%
  select(cong:x2) %>%
  dplyr::filter((party == 100 | party == 200) & cong >= start_cong) %>%
  dplyr::filter(state != "USA") %>% # remove presidents
  mutate(party2 = as.factor(ifelse(party == 100, "D","R"))) %>%
  select(-party) %>%
  rename(party = party2) %>%
  mutate(last_ideo = NA)




