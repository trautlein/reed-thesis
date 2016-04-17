library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(readr)
library(DT)
options(scipen = 10, digits = 5)

start_cong <- 82 # starting congress I want
d <- read.csv("data/SenateData.csv") %>%
  select(cong:x2, role)

# testing out public key addition

d1 <- d %>% # strips out 3rd party, before cong 60, make party variable D or R instead of 100 or 200
  mutate(leader = ifelse(role == "", "N", "Y")) %>%
#  mutate(leader_now = ifelse(role != "W" | "L" | "WL" | "LN", "0", "1")) %>%
  dplyr::filter((party == 100 | party == 200) & cong >= start_cong) %>%
  dplyr::filter(state != "USA") %>% # remove presidents
  mutate(party2 = as.factor(ifelse(party == 100, "D","R"))) %>%
  select(-party) %>%
  rename(party = party2)

d1


party_means_table <- d1 %>% 
  group_by(cong, party) %>%
  summarise(mean_ideo = mean(ideo))

party_means_wide <- party_means_table %>%
  spread(key = party, value = mean_ideo)



lookup <- function(df, val1, val2, col = 3){
  df[df[1] == val1 & df[2] == val2, col][1]
}



party_means_wide$D_del <- vector(mode = "numeric", length = nrow(party_means_wide))
party_means_wide$R_del <- vector(mode = "numeric", length = nrow(party_means_wide))
party_means_wide$D_del[1] <- NA
party_means_wide$R_del[1] <- NA
for(i in 2:nrow(party_means_wide)){
  party_means_wide$D_del[i] <- party_means_wide$D[i] - party_means_wide$D[i - 1]
  party_means_wide$R_del[i] <- party_means_wide$R[i] - party_means_wide$R[i - 1]
}



party_means_dif <- party_means_wide %>%
  select(cong, D_del, R_del) %>%
  rename(D = D_del, R = R_del) %>%
  gather(key = party, value = party_del, D:R)


d2 <- d1

merged <- d2 %>% inner_join(party_means_table, by = c("cong", "party")) %>%
  inner_join(party_means_dif, by = c("cong", "party"))
merged <- merged %>%
  mutate(last_cong = ifelse(cong > start_cong, cong - 1, NA))
merged2 <- merged %>% left_join(party_means_table, by = c("last_cong" = "cong", "party")) %>%
  rename(cong_ideo_mean = mean_ideo.x, last_cong_ideo_mean = mean_ideo.y) %>%
  select(-last_cong)


datatable(merged2, options = list(autoWidth = TRUE))

merged3 <- merged2 %>%
  mutate(ideo_dif = cong_ideo_mean - ideo) %>%
  mutate(last_ideo = NA)

lm1 <- lm(ideo_dif ~ 
                  cong_ideo_mean + # party mean
                  last_cong_ideo_mean + # last party mean
                  
                  leader, # are they a leader
   data = merged3)

summary(lm1)



last_ideo_lookup <- function(df, cong, icpsr, col = 6){
  df[df[1] == cong & df[2] == icpsr, col][1]
}





testmerge <- dplyr::filter(merged3, icpsr == 49703)
datatable(testmerge, options = list(autoWidth = TRUE))
for (j in 1:nrow(testmerge)) {
  testmerge[j,]$last_ideo <- lookup(testmerge, testmerge[j,]$cong - 1, testmerge[j,]$icpsr, 6)
  print(j)
}

datatable(testmerge, options = list(autoWidth = TRUE))


lookup(testmerge, 109, 49703, 6)



for (j in 1:nrow(merged3)) {
  merged3[j,]$last_ideo <- lookup(merged3, merged3[j,]$cong - 1, merged3[j,]$icpsr, 6)
  print(j)
}



merged_D <- dplyr::filter(merged3, party == "D")
merged_R <- dplyr::filter(merged3, party == "R")

lmD <- lm(ideo ~ 
            cong_ideo_mean + # party mean
            last_cong_ideo_mean + # last party mean
            last_ideo + # last ideology score
            leader, # are they a leader
          data = merged_D)

summary(lmD)

lmR <- lm(ideo ~ 
            cong_ideo_mean + # party mean
            last_cong_ideo_mean + # last party mean
            last_ideo + # last ideology score
            leader, # are they a leader
          data = merged_R)

summary(lmR)

