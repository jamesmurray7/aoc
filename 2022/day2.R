data <- as.data.frame(read.table('input/day2.txt'))
names(data) <- c('f', 's') # first / second


# Part 1 ------------------------------------------------------------------

# set up target df.
to.win <- data.frame(
  f = LETTERS[1:3],          # A rock B paper C scissors
  win = c('Y', 'Z', 'X'),    # X rock Y paper Z scissors
  draw = c('X','Y','Z'),      
  lose = c('Z', 'X', 'Y')
)

data2 <- left_join(data, to.win, 'f')

# Score for what move taken, which you get regardless.
move.points <- sum(sapply(data2$s, function(x){
  if(x=='X')
    return(1)
  if(x=='Y')
    return(2)
  if(x=='Z')
    return(3)
}))

# Work out win/draw scores
won <- sum(with(data2, s==win)) * 6
draw <- sum(with(data2, s==draw)) * 3

# Total points then
won + draw + move.points


# Part 2 ------------------------------------------------------------------

# X means you need to lose, Y means you need to end the round in a draw, 
# and Z means you need to win. Good luck!"

data <- as.data.frame(read.table('input/day2.txt'))
names(data) <- c('f', 's') # first / second

target <- data.frame(
  s = LETTERS[24:26],     # X Y Z
  out = c('lose', 'draw', 'win')
)

target

data2 <- left_join(data, target, 's') %>% left_join(., to.win, 'f')

# Number of wins expected
win.points <- sum(data2$out=='win') * 6
# Number of draws expected
draw.points <- sum(data2$out=='draw') * 3

# Points for move chosen
move.taken <- apply(data2, 1, function(x){
  out <- x['out']
  move <- x[out]
})

move.points <- sum(sapply(move.taken, function(x){
  if(x=='X')
    return(1)
  if(x=='Y')
    return(2)
  if(x=='Z')
    return(3)
}))

# Total then
move.points + win.points + draw.points
