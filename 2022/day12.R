library(igraph)

mat <- do.call(rbind, strsplit(readLines('input/day12.txt'), ''))
start.point <- which(mat=='S', arr.ind = T)
end.point <- which(mat=='E', arr.ind = T)

mat[mat=="S"] <- "a"
mat[mat=="E"] <- "z"


coords <- expand.grid(i=1:nrow(mat),j=1:ncol(mat))

edges <- apply(coords, 1, function(x){
  # This row
  i <- x[1]; j <- x[2];
  # This letter (+ as integer)
  this.letter <- mat[i,j]
  this.letter.int <- which(letters==this.letter)
  # Coordinates of candidate moves
  surrounding.letter.coords <- rbind(
    c(i - 1, j),
    c(i + 1, j),
    c(i, j - 1),
    c(i, j + 1)
  )
  # print(surrounding.letter.coords)
  # Check moves are 'on the grid'
  surrounding.letter.coords <- surrounding.letter.coords[!surrounding.letter.coords[,1]%in%c(0, nrow(mat) + 1) &
                                                         !surrounding.letter.coords[,2]%in%c(0, ncol(mat) + 1),]
  # Map these to the letters
  surrounding.letters <- apply(surrounding.letter.coords, 1, function(x){
    m <- mat[x[1],x[2]]
    which(letters==m)
  })
  if("list"%in%class(surrounding.letters)) surrounding.letters <- do.call(c, surrounding.letters)
  # Only valid moves are within one increment
  surrounding.letter.coords <- surrounding.letter.coords[surrounding.letters - this.letter.int <= 1,,drop=F]
  
  # Any valid moves?
  vs <- nrow(surrounding.letter.coords) >= 1

  if(vs){
    from = which(coords[, 1] == i & coords[, 2] == j)
    to = apply(surrounding.letter.coords, 1, function(x){
      which(coords[,1]==x[1] & coords[,2] == x[2])})
    if("list"%in%class(to))to <- do.call(c, to)
  return(data.frame(
    "from" = from,
    "to" = to
  ))
  }else{
    return(NULL)
  }
})

# Part 1 ------------------------------------------------------------------
graph <- graph_from_edgelist(as.matrix(do.call(rbind, edges)))
# plot(graph)
distances(graph, 
          which(coords[,1]==start.point[1] & coords[,2]==start.point[2]),
          which(coords[,1]==end.point[1] & coords[,2]==end.point[2]),
          mode = 'out')
# 472

# Part 2 ------------------------------------------------------------------

# Find which ones are elevation a
elev.a <- apply(which(mat=='a', arr.ind = T), 1, function(x){
  which(coords[,1]==x[1] & coords[,2]==x[2])
})
dists <- distances(graph,
          elev.a,  which(coords[,1]==end.point[1] & coords[,2]==end.point[2]),
          mode = 'out')
plot(dists[!is.infinite(dists)], pch = 20, col = 'magenta')
min(dists) # 465
