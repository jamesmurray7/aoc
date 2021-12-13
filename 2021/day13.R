input <- readLines("day13.txt")
splt <- which(input=="")
points <- read.table(text=input[1:(splt - 1)], sep = ","); colnames(points) <- c('x', 'y')
folds <- read.table(text=input[(splt + 1):length(input)], sep = "=")
mat <- matrix(0, nrow = max(points$x) + 1, ncol = max(points$y) + 1)
for(i in 1:nrow(points)){
  mat[points[i, 1] + 1, points[i, 2] + 1] <- 1
}
# part a)
# Fold along ...
folds[1,] # x = 655
is.x <- function(s) grepl('x$', s)
is.y <- function(s) grepl('y$', s)

fold <- function(m, inst){
  line <- inst[,2]
  if(is.x(inst[, 1])){ # along x
    pre.fold <- m[1:line, ]
    folded <- pre.fold | m[nrow(m):(line + 2), ] # point exists either pre OR post fold 
  }else if(is.y(inst[, 1])){ # along y
    pre.fold <- m[, 1:line]
    folded <- pre.fold | m[, ncol(m):(line + 2)]
  }else{
    print('X or Y not working')
    print(m)
    print(inst)
  }
  folded
}
# Fold once
sum(fold(mat, folds[1,]))

# Part b) Fold all times
mat.to.fold <- mat
for(f in 1:nrow(folds)){
  new.mat.to.fold <- fold(mat.to.fold, folds[f,])
  mat.to.fold <- new.mat.to.fold
}
image(mat.to.fold) # flipped?
image(apply(mat.to.fold, 2 ,rev)) # upside down?
# RPCKFBLR

