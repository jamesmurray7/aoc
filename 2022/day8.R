data <- readLines('input/day8.txt', c(99,99))

# Make the grid -----------------------------------------------------------
mat <- do.call(rbind, lapply(1:length(data),function(x){
  as.integer(el(strsplit(data[x], '')))
}))
image(mat)

# Part 1 ------------------------------------------------------------------
visible <-  2 * sum(dim(mat)) - 4 # outer edge
for(i in 2:(nrow(mat)-1)){
  for(j in 2:(ncol(mat)-1)){
    this.tree <- mat[i, j] # height of this tree
    # Checks (could do this all at once!)
    check.up    <- all(mat[1:(i - 1), j] < this.tree)
    check.down  <- all(mat[(i+1):nrow(mat), j] < this.tree)
    check.left  <- all(mat[i, 1:(j-1)] < this.tree)
    check.right <- all(mat[i, (j+1):ncol(mat)] < this.tree)
    if(check.up | check.down | check.right | check.left) visible <- visible + 1
  }
}
visible # 1816


# Part 2 ------------------------------------------------------------------
scenic <- matrix(0, nrow(mat), ncol(mat)) 
# Outer trees won't be most scenic, so just leave as zero

for(i in 2:(nrow(mat)-1)){
  for(j in 2:(ncol(mat)-1)){
    this.tree <- mat[i, j] # height of this tree
    # Need to work out the number of trees until view gets block
    # in each direction.
    # Left -->
    left <- which(rev(mat[i,1:(j - 1)]) >= this.tree)[1]
    # Right -->
    right <- which(mat[i, (j + 1):ncol(mat)] >= this.tree)[1]
    # Up -->
    up <- which(rev(mat[1:(i - 1), j]) >= this.tree)[1]
    # Down -->
    down <- which(mat[(i+1):nrow(mat), j] >= this.tree)[1]
    # Together
    dirs <- c(left=left,right=right,up=up,down=down)
    if(any(is.na(dirs))){ # There is _for sure_ a better way to do this!
      if(is.na(dirs['left'])) dirs['left'] <- j - 1
      if(is.na(dirs['right'])) dirs['right'] <- ncol(mat)-j
      if(is.na(dirs['up'])) dirs['up'] <- i-1
      if(is.na(dirs['down'])) dirs['down'] <- nrow(mat)-i
    }
    scenic[i, j] <- prod(dirs)
  }
}
max(scenic) # 383520