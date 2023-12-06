(dat <- readLines("input/day3.txt"))
# Making the grid
G <- do.call(rbind, lapply(1:length(dat), function(x) el(strsplit(dat[x],''))))
nr <- nrow(G); nc <- ncol(G)

# Part 1 ------------------------------------------------------------------
symbs <- unique(unlist(apply(G,2,unique)))
symbs <- symbs[!symbs%in%c(as.character(0:9), '.')]

# Symbol locations to parse through and see if any number is adjacent.
where.symbs <- do.call(rbind, lapply(seq_along(symbs), function(i){
  which(G==symbs[i], arr.ind = T)
}))

# Working out adjacent coordinates
mat1 <- expand.grid(-1:1,-1:1)
mat1 <- mat1[!(mat1$Var1==0&mat1$Var2==0),]
nrm <- nrow(mat1)

adjacent.coords <- function(ind){
  out <- apply(t(ind),2,rep,nrm) + mat1
  names(out) <- names(ind)
  out[out$row>=1 & out$row<= nr & out$col >= 1 & out$col <= nc,]
}

# For each adjacent coordinate, seeing if any number is there.
nums <- as.character(0:9)
check.adjacent <- function(ind){
  out <- numeric(2)
  # Below and above
  to.check <- adjacent.coords(ind)
  G[cbind(to.check$row, to.check$col)]
  
}
