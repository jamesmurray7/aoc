fish <- as.integer(do.call(c, strsplit(readLines('./data/day6.txt'), ',')))

# part a)
# Each day, a 0 becomes a 6 and adds a new 8 to the end of the list, 
# while each other number decreases by 1 if it was present at the start of the day.
fishi <- fish
for(i in 1:80){
  fishi <- fishi - 1
  if(any(fishi < 0)){
    newfish <- length(which(fishi < 0))
    fishi <- c(fishi, rep(8, newfish))
    fishi[fishi < 0] <- 6
  }
}
length(fishi)

# part b)
# After 256 days?
fishi <- fish
for(i in 1:256){
  fishi <- fishi - 1
  if(any(fishi < 0)){
    newfish <- length(which(fishi < 0))
    fishi <- c(fishi, rep(8, newfish))
    fishi[fishi < 0] <- 6
  }
  print(i) # hang-up around 160 as vectors too large...
}
# Not feasible for vectors of such size...
# matrix of counts? but then would need to reduce 
# table 0:8
tabfish <- c(0, table(fish), 0, 0, 0)
names(tabfish) <- paste(0:8)
# rolling counts,                            
for(i in 1:256){
  tabfish <- c(tabfish[2:7],            # shift 1:6 -> 0:5
               tabfish[8] + tabfish[1], # number of sixes is number of zeros + number of sevens
               tabfish[9],              # shift 8 -> 7
               tabfish[1])              # shift 0 -> 8
}
sum(tabfish)
