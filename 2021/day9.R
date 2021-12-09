mat <- do.call(rbind, lapply(strsplit(readLines('./data/day9.txt'),''), as.numeric))
mat.store <- mat

# part a)
# check left, right, down, up
# i: row, j: col
outmat <- matrix(F, nrow(mat), ncol(mat))

checkleft <- function(i, j){  # only done if j != 1
  mat[i,j] < mat[i, j - 1]
}
checkright <- function(i, j){ # only done if j != ncol(mat)
  mat[i, j] < mat[i, j + 1]
}
checkup <- function(i, j){    # only done if i != 1
  mat[i, j] < mat[i - 1, j]
}
checkdown <- function(i, j){  # only done if i != nrow(mat)
  mat[i, j] < mat[i + 1, j]
}

# So we don't have to code special cases
mat <- rbind(100, rbind(mat, 100))
mat <- cbind(100, cbind(mat, 100))

for(i in 1:nrow(outmat)){
  for(j in 1:ncol(outmat)){
    outmat[i, j] <- checkleft(i + 1, j + 1) & checkright(i + 1, j + 1) & checkup(i + 1, j + 1) & checkdown(i + 1, j + 1)
  }
}

low.points <- which(outmat, arr.ind = T)
sum(mat.store[low.points] + 1) # 588


# part b)
# All points < 9 are part of some basin, and lead towards nearest low.point(s).

library(raster)
library(igraph)
adj <- raster(mat.store!=9)
basins <- as.matrix(clump(adj, directions = 4)) # a matrix of basins
basins.coordinates <- lapply(1:max(basins, na.rm = T), function(x) which(basins == x, arr.ind = T))
basins.size <- lapply(basins.coordinates, nrow)
prod(sort(do.call(c, basins.size), decreasing = T)[1:3])
image(basins, col = hcl.colors(max(basins,na.rm=T)))
