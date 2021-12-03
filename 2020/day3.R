# Day 3 -------------------------------------------------------------------
rm(list=ls())
d <- as.matrix(read.table('./data/day3.txt', comment.char = '@'))
d <- do.call(rbind, str_split(d[, 1], ''))

# "The same pattern repeats to the right many times:" 
d100 <- replicate(100, d, simplify = F)
d <- do.call(cbind, d100)

# part a)
run <- c()
this.row <- 1; this.col <- 1
this.pos <- d[this.row, this.col]
for(i in 1:(nrow(d) - 1)){
  next.row <- this.row + 1
  next.col <- this.col + 3
  next.pos <- d[next.row, next.col]
  isTREE <- ifelse(next.pos=='#', 'TREE', 'SAFE')
  run[i] <- isTREE
  message('Old position: d[', this.row, ', ', this.col, ']. New position: d[', next.row, ', ', next.col,
          ']\nWith value ', isTREE)
  this.pos <- next.pos; this.row <- next.row; this.col <- next.col
}
length(which(run == 'TREE')) # 198, correct

# part b)
# probably best to define a function
rightxdowny <- function(x, y){
  message('Going ACROSS by ', x, 'and DOWN by ', y, 'at each iteration.')
  run <- c()
  this.row <- 1; this.col <- 1
  this.pos <- d[this.row, this.col]
  for(i in 1:nrow(d)){
    next.col <- this.col + x
    next.row <- this.row + y
    next.pos <- d[next.row, next.col]
    isTREE <- ifelse(next.pos=='#', 'TREE', 'SAFE')
    run[i] <- isTREE
    message('Old position: d[', this.row, ', ', this.col, ']. New position: d[', next.row, ', ', next.col,
            ']\nWith value ', isTREE)
    this.pos <- next.pos; this.row <- next.row; this.col <- next.col
    if(this.row + y > nrow(d)) break;
  }
  as.numeric(length(which(run == 'TREE')))
}

r3d1 <- rightxdowny(3, 1) # same as part a
r1d1 <- rightxdowny(1, 1) 
r5d1 <- rightxdowny(5, 1)
r7d1 <- rightxdowny(7, 1)
r1d2 <- rightxdowny(1, 2)
r3d1 * r1d1 * r5d1 * r7d1 * r1d2 # 5140884672. Correct