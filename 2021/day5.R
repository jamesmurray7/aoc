# part a)
lines <- readLines('./data/day5.txt')
df <- as.data.frame(do.call(rbind, strsplit(lines, ',|( -> )')))
df <- dplyr::mutate_all(df, as.numeric)
colnames(df) <-  c('x1', 'y1', 'x2', 'y2')

grid.size <- max(apply(df, 2, max))
grid <- matrix(0, nr = grid.size, nc = grid.size)
for(i in 1:nrow(df)){
  x.move = seq(df[i, 'x1'], df[i, 'x2'], 1)
  y.move = seq(df[i, 'y1'], df[i, 'y2'], 1)
  grid[x.move, y.move] <- grid[x.move, y.move] + 1
} # this not working. 

which(grid > 1, arr.ind = T)

# wrong, need to get vertical/horizontal only...
df2 <- subset(df, x1 == x2 | y1 == y2) 
x <- mapply(seq, df2$x1, df2$x2)  # x movement
y <- mapply(seq, df2$y1, df2$y2)  # y movement
all.lines <- as.data.frame(do.call(rbind, mapply(cbind, x, y))) # all points crossed 
sum(table(all.lines) > 1)

# part b the same but without the subsetting step, and what i did originally...
df2 <- df #lazy
x <- mapply(seq, df2$x1, df2$x2)
y <- mapply(seq, df2$y1, df2$y2)
all.lines <- as.data.frame(do.call(rbind, mapply(cbind, x, y)))
sum(table(all.lines) > 1)
