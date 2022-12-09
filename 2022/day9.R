data <- read.table('input/day9.txt', sep = ' ')
names(data) <- c('dir', 'move')
data$move <- as.integer(data$move)

h <- t <- matrix(0, 1, 2)

for(i in 1:nrow(data)){
  hmove <- switch(data[i, 'dir'],
                  U = c(0,  1),
                  D = c(0, -1),
                  L = c(-1, 0),
                  R = c( 1, 0)
  )
  m <- data[i, 'move']
  while(m > 0){
    h <- rbind(h, h[nrow(h), ] + hmove)
    m <- m - 1
  }
  # not a smart way of doing this
  # h <- rbind(h, t(replicate(abs(hmove[abs(hmove)>0]), 
  #                           hmove/data[i,'move'])))
}

# Checking position of t relative to h
t.to.h <- function(h, t){
  x <- h - t
  if(any(abs(x) > 1)){
    if(abs(x[1]) >= 1) 
      horiz.move <- sign(x[1]) * 1
    else
      horiz.move <- 0
    if(abs(x[2]) >= 1)
      vert.move <- sign(x[2]) * 1
    else
      vert.move <- 0
    out <- c(horiz.move, vert.move)
  }else{
    out <- c(0, 0)
  }
  out
}


for(i in 1:nrow(h)){
  t <- rbind(t, t[nrow(t),] + t.to.h(h[i, ], t[nrow(t),]))
}

nrow(t[!duplicated(t),]) # 5683
