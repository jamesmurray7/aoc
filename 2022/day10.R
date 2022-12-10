data <- readLines('./input/day10.txt')

ninst <- length(data)
# X starts with value 1.
signal <- numeric(1) + 1

for(i in 1:ninst){
  if(data[i] == 'noop'){
    signal <- c(signal, signal[length(signal)])
  }else{
    change <- as.numeric(gsub('addx', '', data[i])) # addx yy
    signal <- c(signal, signal[length(signal)], signal[length(signal)] + change)
  }
}

# Part 1 ------------------------------------------------------------------
# Find the signal strength during the 20th, 60th, 100th, 140th, 
# 180th, and 220th cycles
times <- c(20, 60, 100, 140, 180, 220)
sum(signal[times] * times) #13600


# Part 2 ------------------------------------------------------------------
# 40 wide and 6 high
# lit pixel (#); otherwise, the screen leaves the pixel dark (.)

screen <- matrix(0:39, nr = 6, nc = 40, byr = T)
crt.screen <- matrix('.', nr = 6, nc = 40)
p <- 1
for(i in 1:6){
  for(j in 1:40){
    cat(sprintf('i is %d, j is %d, p is %d\n',i,j,p))
    if(abs(signal[p] - screen[i,j]) <= 1)
      crt.screen[i,j] <- '#'
    p <- p + 1
  }
}

cat(paste(apply(crt.screen,1,paste0,collapse=''),collapse='\n'))
image(t(apply(crt.screen, 2, function(x) rev(ifelse(x=='#', 1, 0)))),
      xaxt = 'n', yaxt='n', col = hcl.colors(12, 'BuPu',rev=T))
# PCPBKAPJ