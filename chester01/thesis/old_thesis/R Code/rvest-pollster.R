library("dplyr")
library("rvest")
library("magrittr")

webpage <- html("http://elections.huffingtonpost.com/pollster/polls/quinnipiac-22788")


support <-
webpage %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table()

colnames(support)[2] <- "perc"
colnames(support)[1] <- "candidate"

support$perc <- as.numeric(sub("%", "", support$perc))

arrange(support, desc(perc))

