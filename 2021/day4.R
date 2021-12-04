#' ######
#' Work-in-progress script for solutions to AoC 2020
#' #####
library(tidyverse)
cards <- read.table('./data/day4.txt', blank.lines.skip = F)
nums <- read.csv('./data/day4.csv', header = F)
nums <- unname(do.call(c, c(nums))) 
# part a)
cards2 <- cards[!is.na(cards),]
cardslist <- list()
i <- 1; p <- 1
while(length(cardslist) != 100){
  cardslist[[p]] <- cards2[i:(i+4),]
  i <- i + 5
  p <- p + 1
}


# find bingos
bingos <- lapply(cardslist, function(x){
  out <- list()
  outmat <- matrix(NA, 5, 5)
  used.nums <- c()
  for(j in 1:length(nums)){
    outmat[which(x == nums[j], arr.ind = T)] <- nums[j]
    used.nums[j] <- nums[j]
    rs <- rowSums(outmat); cs <- colSums(outmat)
    if(any(!is.na(rs)) | any(!is.na(cs))){
      break
    }
  }
  list(outmat, used.nums)
})

#find which board had it first
winner <- which.min(lapply(lapply(bingos, '[[', 2), length))
winner

# sum of all unmarked numbers
winning.board <- bingos[[winner]][[1]]
which(is.na(winning.board), arr.ind = T) 
sum(cardslist[[winner]][which(is.na(winning.board), arr.ind = T)]) * tail(bingos[[winner]][[2]],1)

# part b
#which wins last?
loser <- which.max(lapply(lapply(bingos, '[[', 2), length))
losing.board <- bingos[[loser]][[1]]
which(is.na(losing.board), arr.ind = T)
sum(cardslist[[loser]][which(is.na(losing.board), arr.ind = T)]) * tail(bingos[[loser]][[2]], 1)
