setwd('~/Documents/AoC/')
# First part
data <- read.table('Day1Input.txt')
measures <- c(NA, as.vector(data$V1))
measures
sum(diff(measures) > 0, na.rm = T) # 1400

# Second part
# Need to get "window markings" included...
library(tidyverse)
dd <- tibble(reading = data$V1)
dd$n <- 1:nrow(dd)
dd

dd$r <- dd$n %% 3
dd$newgroup <- dd$r == 1
dd$newgroup2 <- dd$r == 2
dd$newgroup3 <- dd$r == 0
dd$newgroup <- cumsum(dd$newgroup)
dd$newgroup2 <- cumsum(dd$newgroup2)
dd$newgroup3 <- cumsum(dd$newgroup3)

A <- B <- C <- list()
for(i in 1:666){
  dd %>% filter(newgroup == i | newgroup2 == i | newgroup3 == i)
  A[[i]] <- sum(dd[dd$newgroup == i, 'reading'])
  B[[i]] <- sum(dd[dd$newgroup2 == i, 'reading'])
  C[[i]] <- sum(dd[dd$newgroup3 == i, 'reading'])
}

# increases from A->B (s.t. B > A)
BA <- sapply(1:666, function(a) B[[a]] > A[[a]])
# increases from B->C (s.t. C > B)
CB <- sapply(1:666, function(a) C[[a]] > B[[a]])
# increases 'across' construction here: A[[current_i + 1] > C[[current_i]]
AC <- sapply(1:665, function(a) A[[a+1]] > C[[a]])

sum(BA) + sum(CB) + sum(AC)
