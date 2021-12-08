setwd('~/Documents/AoC/')
crab <- as.numeric(scan('day7.txt', sep = ','))

# part a
pos <- list(); p <- 1
for(i in seq(range(crab)[1], range(crab)[2])){
  pos[[p]] <- sum(abs(crab - i))
  p <- p + 1
}
pos[[which.min(do.call(c, pos))]]

# part b
pos <- list(); p <- 1
for(i in seq(range(crab)[1], range(crab)[2])){
  moves <- abs(crab - i)
  pos[[p]] <- sum(moves/2 * (moves + 1)) # sum of sequence of numbers (https://en.wikipedia.org/wiki/1_%2B_2_%2B_3_%2B_4_%2B_%E2%8B%AF)
  p <- p + 1
}
pos[[which.min(do.call(c, pos))]]
