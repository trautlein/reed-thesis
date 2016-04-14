#main_code_003_backup.R

library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(readr)
library(DT)

options(scipen = 10, digits = 5)

start_cong <- 82 # starting congress I want
d <- read.csv("data/SenateData002.csv") %>%
  select_("Cong.","ICPSR", "State.Code", "State", "Party", "Name", "X1st.Dim.", "X2nd.Dim.", "Role")


d <- dplyr::rename(d, cong = Cong., icpsr = ICPSR, statecode = State.Code,
                   state = State, party = Party, name = Name, 
                   ideo = X1st.Dim.,x2 = X2nd.Dim., role = Role)



d1 <- d %>% # strips out 3rd party, before cong 60, make party variable D or R instead of 100 or 200
  mutate(leader = ifelse(role == "", NA, "Y")) %>%
  mutate(leader_now = ifelse(role != "Y", "0", "1")) %>%
  dplyr::filter((party == 100 | party == 200) & cong >= start_cong) %>%
  mutate(party2 = as.factor(ifelse(party == 100, "D","R"))) %>%
  select(-party) %>%
  rename(party = party2)


party_means_table <- d1 %>% 
  group_by(cong, party) %>%
  summarise(mean_ideo = mean(ideo))

party_means_wide <- party_means_table %>%
  spread(key = party, value = mean_ideo)

# plot of how mean_ideo by party has changed over congresses
ggplot(data = party_means_table, aes(x = cong, y = mean_ideo, color = party)) + 
  geom_line(size = 1) + geom_point(size = 3) + 
  scale_color_brewer(palette = "Set1") + ylim(c(-1, 1)) +
  scale_color_manual(values=c("blue", "red"))



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




#datatable(merged2, options = list(autoWidth = TRUE))





