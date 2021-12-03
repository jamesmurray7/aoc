# Day 1 -------------------------------------------------------------------
d <- read.table('./data/day1.txt')
# part a)
d <- c(d$V1)
dod <- outer(d, d, FUN = '+')
sum.to.2020 <- which(dod == 2020, arr.ind = T)[1,]
prod(d[unique(sum.to.2020)]) # answer 

# part b)
sum3.to.2020 <- which(outer(dod, d, '+')==2020, arr.ind = T)[1,]
prod(d[sum3.to.2020])