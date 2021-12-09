setwd('~/Documents/AoC/')
# Because the digits 1, 4, 7, and 8 each use a unique number of segments, 
# you should be able to tell which combinations of signals correspond to those digits. 
# Counting only digits in the output values (the part after | on each line), in the above example, 
# there are 26 instances of digits that use a unique number of segments (highlighted above).
# 
# In the output values, how many times do digits 1, 4, 7, or 8 appear?
  
# part a)
# Digit 1 -> 2 chars; 4 -> 4 chars; 7 -> 3 chars; 8 -> 7 chars
input <- trimws(scan('day8.txt','character',sep='|')[seq(2,400,2)])
length(which(nchar(do.call(c, strsplit(input,'\\s'))) %in% c(2, 4, 3, 7)))
nchar(input)


# part b)
# doesnt matter what order they're in, but need to match
sortchars <- function(x){  # x a vector of characters
  out <- c()
  for(j in seq_along(x)){
    out[j] <- paste0(sort(strsplit(x[j], '')[[1]]), collapse = '')
  }
  out
}

# For checking (zero, six, nine) and (three, two) candidates
get.intersect <- function(x, y){
  x <- strsplit(x, '')[[1]]
  y <- strsplit(y, '')[[1]]
  length(intersect(x, y))
}

board <- c()
for(i in 1:length(digits)){
  display <- vector('list', 7) # a->g LETTERS[1:7]
  dd <- digits[i]
  inout <- strsplit(dd, '\\|')
  input <- unlist(strsplit(inout[[1]][1], ' '))
  output <- unlist(strsplit(trimws(inout[[1]][2]), ' '))
  # Sort input/output
  input <- sapply(input, sortchars)
  output <- sapply(output, sortchars)
  
  # Ones
  d1 <- input[which(nchar(input) == 2)]
  input <- setdiff(input, d1)
  
  # Fours
  d4 <- input[which(nchar(input) == 4)]
  input <- setdiff(input, d4)
  
  # Sevens
  d7 <- input[which(nchar(input) == 3)]
  input <- setdiff(input, d7)
  
  # Eights
  d8 <- input[which(nchar(input) == 7)]
  input <- setdiff(input, d8)
  
  # Nines, Zeros and Sixes
  # 9: Keep if nchar == 6 AND intersection with d4 is of length 4 (i.e. same four characters)
  # 0: Keep if nchar == 6 AND intersection with d1 is of length 2 (i.e. same two characters )
  if(any(nchar(input == 6))){
    temp609 <- input[which(nchar(input) == 6)]
    d4.crossover <- sapply(temp609, get.intersect, d4)
    d9 <- temp609[which(d4.crossover == nchar(d4))]
    input <- setdiff(input, d9)
    temp609 <- setdiff(temp609, d9)
    d1.crossover <- sapply(temp609, get.intersect, d1)
    d0 <- temp609[which(d1.crossover == nchar(d1))]
    input <- setdiff(input, d0)
    temp609 <- setdiff(temp609, d0)
    # Six is those remaining
    d6 <- temp609
    input <- setdiff(input, d6)
  }
  
  # Threes and Twos
  # 3: Keep if nchar == 5 AND intersection with d1 is of length two (same two characters)
  # 2: Keep if nchar == 5 AND intersection with d4 is of length two 
  if(any(nchar(input == 5))){
    temp32 <- input[which(nchar(input) == 5)]
    d1.crossover <- sapply(temp32, get.intersect, d1)
    d3 <- temp32[which(d1.crossover == nchar(d1))]
    input <- setdiff(input, d3)
    temp32 <- setdiff(temp32, d3)
    d4.crossover <- sapply(temp32, get.intersect, d4)
    d2 <- temp32[which(d4.crossover == 2)]
    input <- setdiff(input, d2)
    temp32 <- setdiff(temp32, d2)
  }
  
  d5 <- input 
  
  inorder <- unname(c(d0, d1, d2, d3, d4, d5, d6, d7, d8, d9))
  
  display <- 0:9; names(display) <- inorder
  
  # ##
  # Compare with output
  # ##
  
  numericoutput <- as.numeric(paste0(match(output, names(display)) - 1, collapse = ''))
  board[i] <- numericoutput
}
sum(board)

