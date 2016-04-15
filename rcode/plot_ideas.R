#cool plot ideas

#### SENATE ####
# plot of how mean_ideo by party has changed over congresses
ggplot(data = party_means_table, aes(x = cong, y = mean_ideo, color = party)) + 
  geom_line(size = 1) + geom_point(size = 3) + 
  scale_color_brewer(palette = "Set1") + ylim(c(-1, 1)) +
  scale_color_manual(values=c("blue", "red"))


