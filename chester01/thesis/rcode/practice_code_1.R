setwd("~/Insync/thesis/")
library(dplyr)
library(ggplot2)

d <- read.csv("SenateData002.csv")

names(d)
d$District <- NULL
d$X2nd.Dim. <- NULL
d$Log.Likelihood <- NULL
d$Votes <- NULL
d$Classification.Errors <- NULL
d$Power <- NULL
d$Geometric.Mean.Probability <- NULL
d$Leader <- NULL
head(d)


leahy <- dplyr::filter(d, ICPSR == 14307)

ggplot(leahy, aes(Cong., X1st.Dim.)) + geom_point()

ggplot(dplyr::filter(d, Cong. > 60), aes(Cong., X1st.Dim.)) + geom_point(aes(color = factor(Party)))

cong113 <- dplyr::filter(d, Cong. == 113)

ggplot(cong113, aes(X1st.Dim.)) + geom_histogram()

mean(cong113$X1st.Dim.)
