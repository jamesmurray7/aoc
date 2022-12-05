# Parsing data -- crates first
data <- readLines('input/day5.txt')
brk <- which(data == "")
crates <- read.fwf('input/day5.txt', widths = rep(4, 9), n = (brk-2))

crates <- apply(crates, 2, function(i) trimws(gsub('\\[|\\]', '', i)))

crates2 <- vector('list', ncol(crates))
for(i in 1:ncol(crates)){
  crates2[[i]] <- crates[,i]
  crates2[[i]] <- crates2[[i]][crates2[[i]] != ""]
}

# Moves
moves <- data[(brk+1):length(data)] 
moves <- as.data.frame(do.call(rbind, stringr::str_extract_all(moves, '[0-9]+')))
# In format <NUMBER TO MOVE> <FROM> <TO>.
names(moves) <- c('num', 'from', 'to')
moves <- apply(moves, 2, as.integer)

# Part 1 ------------------------------------------------------------------
for(m in 1:nrow(moves)){
  num <- moves[m, "num"]; from <- moves[m, "from"]; to <- moves[m, "to"]
  # Set which ones need to be moved.
  to.move <- crates2[[from]][1:num]
  # REMOVE THESE from stack
  crates2[[from]] <- crates2[[from]][-(1:num)]
  # Place these on top of the "TO" stack.
  crates2[[to]] <- c(rev(to.move), crates2[[to]])
}

paste0(sapply(crates2, function(x) x[1]),collapse='') # "RNZLFZSJH"


# Part 2 ------------------------------------------------------------------
# The exact same, but not reversing to.move in loop below.
crates2 <- vector('list', ncol(crates))
for(i in 1:ncol(crates)){
  crates2[[i]] <- crates[,i]
  crates2[[i]] <- crates2[[i]][crates2[[i]] != ""]
}

for(m in 1:nrow(moves)){
  num <- moves[m, "num"]; from <- moves[m, "from"]; to <- moves[m, "to"]
  # Set which ones need to be moved.
  to.move <- crates2[[from]][1:num]
  # REMOVE THESE from stack
  crates2[[from]] <- crates2[[from]][-(1:num)]
  # Place these on top of the "TO" stack.
  crates2[[to]] <- c(to.move, crates2[[to]])
}

paste0(sapply(crates2, function(x) x[1]),collapse='') # "CNSFCGJSM"
