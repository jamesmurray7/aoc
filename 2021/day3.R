library(tidyverse)
d <- tibble(read.table('~/Documents/AoC/Day3.txt', colClasses = 'character'))
names(d) <- 'input'
# part a)
nc <- unique(nchar(d$input))
dsplit <- tibble(do.call(rbind, lapply(apply(d, 1, strsplit, ''), '[[', 1)))

counts <- apply(dsplit, 2, table)
# gamma - most common
gammavec <- as.vector(apply(counts, 2, which.max)-1) # most common -> 'gamma'
gamma <- 1816 # 1816 is gamma
# epsilon - least common
epsvec <- as.vector(apply(counts,2,which.min)-1)
epsilon <- 2279 # found using a website!

epsilon * gamma

# part b)
# Oxygen ----
dsplit <- tibble(do.call(rbind, lapply(apply(d, 1, strsplit, ''), '[[', 1)))
dsplit <- apply(dsplit, 2, as.integer)
names(dsplit) <- NULL

get.most.common <- function(x){
  xx <- table(x)
  if(xx[2]==xx[1]){
    rtn <- as.numeric(names(xx[2]))
  }else{
    rtn <- as.numeric(names(which.max(xx)))
  }
  rtn
}

get.least.common <- function(x){
  xx <- table(x)
  if(xx[2]==xx[1]){
    rtn <- as.numeric(names(xx[1]))
  }else{
    rtn <- as.numeric(names(which.min(xx)))
  }
  rtn
}

ot <- ct <- temp <- dsplit # o: oxygen, c: co2
i <- 1
while(nrow(ot)>1){
  most.common.i <- get.most.common(ot[, i])
  ot <- ot[ot[,i]==most.common.i, , drop = F]
  i <- i+1
}
i <- 1
while(nrow(ct)>1){
  least.common.i <- get.least.common(ct[, i])
  ct <- ct[ct[,i]==least.common.i, , drop = F]
  i <- i+1
}


# oxygen
apply(ot,1,paste0, collapse = '') #2031
#co2
apply(ct,1,paste0, collapse = '') #2104
2031 * 2104
