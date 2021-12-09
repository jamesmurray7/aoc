seats <- readLines('data/day5.txt')

# part a)
# function for splitting sequence
front.back <- function(start, stop, FB = ''){
  if(!FB %in% c('F', 'B', 'L', 'R')) stop('something wrong with FB')
  if(FB %in% c('F', 'L')) section <- 1 else section <- 2
  split(seq(start, stop), rep(c(1,2), each = length(seq(start, stop))/2))[[section]]
}

rows <- cols <- c()
for(i in 1:length(seats)){
  s <- seats[i]
  ss <- unlist(strsplit(s, ''))
  start <- 0; stop <- 127
  for(j in 1:7){
    sec <- front.back(start, stop, paste0(ss[j]))
    start <- sec[1]; stop <- sec[length(sec)]
  }
  rows[i] <- sec
  start <- 0; stop <- 7
  for(j in 1:3){
    sec <- front.back(start, stop, paste0(ss[j + 7]))
    start <- sec[1]; stop <- sec[length(sec)]
  }
  cols[i] <- sec
}

seat.ids <- 8 * rows + cols
max(seat.ids) # 913

# part b)
sorted.seat.ids <- sort(seat.ids)
my.ind <- which(diff(c(sorted.seat.ids)) > 1)
sorted.seat.ids[c(my.ind-2, my.ind-1, my.ind, my.ind+1, my.ind+2)] # 717 missing
      