x <- readLines('day10.txt')
# part a
opens <- c('(', '[', '{', '<')
close <- c(')', ']', '}', '>')

unmatched <- sapply(x, function(s){
  while(grepl('(\\(\\))|(\\{\\})|(\\[\\])|(<>)', s)){
    s <- gsub('(\\(\\))|(\\{\\})|(\\[\\])|(<>)', '', s)
  }
  gsub('^[[({<]+', '', s)
})

corrupt.score <- sapply(unmatched[unmatched!=''], function(s){
  head(strsplit(s, '')[[1]], 1)
})

names(close) <- c(3, 57, 1197, 25137)
sum(as.numeric(names(close)[match(corrupt.score, close)]))

# part b
incomplete <- names(unmatched[unmatched == ''])
unmatched.incompletes <- sapply(incomplete, function(s){ # can repeat first bit of above to reduce down to unmatched part
  while(grepl('(\\(\\))|(\\{\\})|(\\[\\])|(<>)', s)){
    s <- gsub('(\\(\\))|(\\{\\})|(\\[\\])|(<>)', '', s)
  }
  s
})
# unmatched.incompletes just opening characters
all(sapply(unmatched.incompletes, function(x){
  any(strsplit(x, '')[[1]] %in% opens)
})) #T

# and now the order they need to be matched to (-- '(', '[', '{', '<' --)
library(tidyverse)
scorecard <- tibble(opens, close, score = 1:4)
get.total.score <- function(x){
  scores <- left_join(data.frame(opens = strsplit(x, '')[[1]]), scorecard, 'opens') %>% 
    as_tibble 
  #print(scores)
  total.score <- 0
  for(i in seq_along(scores$score)){
    #print(i)
    total.score <- 5 * total.score + rev(scores$score)[i]
    #print(total.score)
  }
  total.score
}

completion.scores <- sort(unname(sapply(unmatched.incompletes, get.total.score)))
median(completion.scores) # 4361305341






