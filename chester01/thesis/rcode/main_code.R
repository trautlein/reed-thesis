library(ggplot2)
library(dplyr)
library(tidyr)

options(scipen = 10)

start_cong <- 82 # starting congress I want
d <- read.csv("data/SenateData002.csv")
d <- d[,c("Cong.","ICPSR", "State.Code", "State", "Party", "Name", "X1st.Dim.", "X2nd.Dim.", "Role")]

d <- dplyr::rename(d, cong = Cong., icpsr = ICPSR, statecode = State.Code,
                   state = State, party = Party, name = Name, 
                   ideo = X1st.Dim.,x2 = X2nd.Dim., role = Role)

d1 <- d %>% # strips out 3rd party, before cong 60, make party variable D or R instead of 100 or 200
  mutate(leader = ifelse(role == "", NA, "Y")) %>%
  filter((party == 100 | party == 200) & cong >= start_cong) %>%
  mutate(party2 = ifelse(party == 100, "D","R")) %>%
  select(-party) %>%
  rename(party = party2)

party_means_table <- d1 %>% 
  group_by(cong, party) %>%
  summarise(mean_ideo = mean(ideo))

party_means_wide <- party_means_table %>%
  spread(key = party, value = mean_ideo)

lookup <- function(df, val1, val2, col = 3){
  df[df[1] == val1 & df[2] == val2, col][1]
}

party_means_wide <- party_means_wide %>%
  mutate(D_lagged = lag(D, k = 1),
    R_lagged = lag(R, k = 1),
    D_del = D - D_lagged,
    R_del = R - R_lagged)

party_means_dif <- party_means_wide %>%
  select(cong, D_del, R_del) %>%
  rename(D = D_del, R = R_del) %>%
  gather(key = party, value = party_del, D:R)

d2 <- d1

for(i in seq_along(nrow(d1))){
  d2$party_cong_mean[i] <- lookup(df = party_means_table, 
    val1 = d1$cong[i],
    val2 = d1$party[i])
  d2$party_cong_mean_dif[i] <- lookup(df = party_means_dif, 
    val1 = d1$cong[i], 
    val2 = d1$party[i])
  d2$last_cong_ideo[i] <- ifelse(d1$cong[i] > start_cong, 
    lookup(df = party_means_table, 
      val1 = d1$cong[i] - 1, 
      val2 = d1$party[i]),
    NA)
}











ggplot(dplyr::filter(d2, cong > 90), aes(cong, ideo, color = party)) + geom_point() +
  scale_color_manual(values=c("blue", "red"))
