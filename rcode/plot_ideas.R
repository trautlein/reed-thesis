#cool plot ideas


#### LOAD CODE ####
senate_D <- read.csv("data/Senate_D.csv", header = TRUE)
senate_R <- read.csv("data/Senate_R.csv", header = TRUE)
house_D <- read.csv("data/House_D.csv", header = TRUE)
house_R <- read.csv("data/House_R.csv", header = TRUE)


senate_lmD <- lm(ideo ~ 
                   cong_ideo_mean + # party mean
                   last_cong_ideo_mean + # last party mean
                   last_ideo + # last ideology score
                   leader, # are they a leader
                 data = senate_D)

senate_lmR <- lm(ideo ~ 
                   cong_ideo_mean + # party mean
                   last_cong_ideo_mean + # last party mean
                   last_ideo + # last ideology score
                   leader, # are they a leader
                 data = senate_R)

house_lmD <- lm(ideo ~ 
                   cong_ideo_mean + # party mean
                   last_cong_ideo_mean + # last party mean
                   last_ideo + # last ideology score
                   leader, # are they a leader
                 data = house_D)

house_lmR <- lm(ideo ~ 
                   cong_ideo_mean + # party mean
                   last_cong_ideo_mean + # last party mean
                   last_ideo + # last ideology score
                   leader, # are they a leader
                 data = house_R)

summary(senate_lmD)
summary(senate_lmR)
summary(house_lmD)
summary(house_lmR)

#### SENATE ####
# plot of how mean_ideo by party has changed over congresses
# ggplot(data = party_means_table, aes(x = cong, y = mean_ideo, color = party)) + 
#   geom_line(size = 1) + geom_point(size = 3) + 
#   scale_color_brewer(palette = "Set1") + ylim(c(-1, 1)) +
#   scale_color_manual(values=c("blue", "red"))
# 
# 
