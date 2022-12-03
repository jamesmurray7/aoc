# Readin
data <- read.table('input/day3.txt')


# Part 1 ------------------------------------------------------------------
# Split each into two halves
data.split <- apply(data, 1, function(i){
  nc <- nchar(i)
  split <- el(strsplit(i, ''))
  first <- split[1:(nc/2)]
  second <- split[(nc/2 + 1):nc]
  intersect(first,second)
})

# Score shared letter and sum.
scores <- setNames(1:(26*2), c(letters,LETTERS))
sum(match(data.split, names(scores)))


# Part 2 ------------------------------------------------------------------

# Elf grouping each 3 lines
data$group <- rep(1:100,each=3)

shared <- with(data, tapply(V1, group, function(i){
  split.i <- sapply(i, function(ii){
    unique(el(strsplit(ii, '')))
  })
  Reduce(intersect, split.i)
}))

sum(match(shared, names(scores)))
     
