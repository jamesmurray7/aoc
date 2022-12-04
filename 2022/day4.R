# Readin
data <- as.data.frame(read.table('input/day4.txt'))

# separate the elves
library(tidyr); library(dplyr)
data2 <- separate(data, V1, c('one', 'two'), ',')


# Part 1 ------------------------------------------------------------------
# Write a function (instead of doing nested separates)
start.range <- function(range) as.numeric(gsub('\\-.*$', '', range))
end.range <- function(range) as.numeric(gsub('^.*\\-', '', range))

# If EITHER range1 starts and ends within range two
# OR range2 starts and ends within range two

check.contained <- function(range1, range2){
  one.start <- start.range(range1); one.end <- end.range(range1)
  two.start <- start.range(range2); two.end <- end.range(range2)
  
  check1 <- one.start >= two.start & one.end <= two.end
  check2 <- two.start >= one.start & two.end <= one.end
  any(check1, check2)
}

sum(apply(data2, 1, function(i){
  check.contained(i[1], i[2])
}))


# Part 2 ------------------------------------------------------------------
# Function to expand a given range
expand.range <- function(range){
  start <- start.range(range); end <-  end.range(range)
  seq(start, end, 1)
}

check.overlap <- function(range1, range2){
  # check4 <- two.end >= one.end0
  e1 <- expand.range(range1)
  e2 <- expand.range(range2)
  any(table(sort(c(e1, e2))) > 1)
}

sum(apply(data2, 1, function(i){
  check.overlap(i[1], i[2])
}))

