library(ggplot2)
library(dplyr)

setwd("~/Insync/thesis/")

d <- read.csv("~/Insync/thesis/data/SenateData002.csv")
d <- d[,c("Cong.","ICPSR", "State.Code", "State", "Party", "Name", "X1st.Dim.", "X2nd.Dim.", "Role")]

d <- dplyr::rename(d, cong = Cong., 
                   icpsr = ICPSR,
                   statecode = State.Code,
                   state = State, 
                   party = Party,
                   name = Name, 
                   ideo = X1st.Dim., 
                   x2 = X2nd.Dim.,
                   role = Role)
start_cong <- 82

d %>% # strips out 3rd party, before cong 60, make party variable D or R instead of 100 or 200
  mutate(leader = ifelse(role == "", NA, "Y")) %>%
  filter((party == 100 | party == 200) & cong >= start_cong) %>%
  mutate(party2 = ifelse(party == 100, "D","R")) %>%
  select(-party) %>%
  rename(party = party2) -> d1

#< Make party_cong_mean variable and ...
party_means <- data.frame(matrix(NA, nrow = 113, ncol = 0))
party_means$D     <- rep(NA, 113)
party_means$R     <- rep(NA, 113)
party_means$R_del <- rep(NA, 113)
party_means$D_del <- rep(NA, 113)

for(i in (start_cong+1):113) {
  party_means$D[i]  <- mean(dplyr::filter(d1, cong == i & party == "D")$ideo)
  party_means$R[i]  <- mean(dplyr::filter(d1, cong == i & party == "R")$ideo)
  party_means$D_del[i] <- party_means$D[i] - party_means$D[i-1]
  party_means$R_del[i] <- party_means$R[i] - party_means$R[i-1]
}

d2 <- dplyr::mutate(d1, party_cong_mean = ifelse(party=="D", 
                                                 party_means$D[cong], 
                                                 party_means$R[cong]))

d2 <- dplyr::mutate(d2, party_cong_mean_dif = ifelse(party=="D", 
                                                 party_means$D_del[cong], 
                                                 party_means$R_del[cong])) #>

#make last_cong_ideo variable

#d2 <- dplyr:: mutate(d2, last_cong_ideo = ifelse(cong > start_cong,
#                                                 ideo[cong - 1],
#                                                 NA))


dR <- d2 %>% filter(party == "R")
dD <- d2 %>% filter(party == "D")



acf(d2$ideo)






#d2 <- dplyr::mutate(d2, person_dif = ideo$cong - ideo$(cong-1))

#for(i in 1:length(d1)) {
#  ifelse(d1$role[i] == "", d1$role <- NA, d1$role <- d1$role)
#}

ggplot(dplyr::filter(d2, cong > 90), aes(cong, ideo, color = party)) + geom_point() +
  scale_color_manual(values=c("blue", "red"))


ggplot(dplyr::filter(d1, cong > 90), aes(cong, ideo, color = party)) + geom_point() +
  scale_color_manual(values=c("blue", "red"))


leahy <- dplyr::filter(d, icpsr == 14307)
ggplot(leahy, aes(cong, ideo)) + geom_point()

ggplot(dplyr::filter(d1, cong >= 110), aes(ideo, x2, color=party)) + geom_point() +
  scale_color_manual(values=c("blue", "red"))

cong113 <- dplyr::filter(d1, cong == 113)
ggplot(cong113, aes(ideo, color = party)) + geom_histogram(binwidth = 0.025)