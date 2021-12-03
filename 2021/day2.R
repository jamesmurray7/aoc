d <- data.frame(read.table('./data/day2.txt'))
names(d) <- c('direction', 'amount')

# part a)

# Start at (0, 0)
# forward X increases the horizontal position by X units.
# down X increases the depth by X units.
# up X decreases the depth by X units.
position <- c(horiz = 0, depth = 0)
for(i in 1:nrow(d)){
  d.i <- d[i, ]
  if(d.i$direction == 'forward') position[1] <- position[1] + d.i$amount
  if(d.i$direction == 'down') position[2] <- position[2] + d.i$amount
  if(d.i$direction == 'up') position[2] <- position[2] - d.i$amount
}
position

# Try a mutate + sum way
d %>% 
  tibble %>% 
  mutate(horiz = ifelse(direction == 'forward', amount, 0),
         depth = case_when(
           direction == 'down' ~ as.integer(amount),
           direction == 'up' ~ as.integer(-1 * amount),
           T ~ as.integer(0.0)
         )) %>% 
  select(horiz, depth) %>% 
  colSums %>% 
  prod

# part b)
# down X increases your aim by X units.
# up X decreases your aim by X units.
# forward X does two things:
#   It increases your horizontal position by X units.
#   It increases your depth by your aim multiplied by X.
rm(position, d.i, i)
d2 <- d %>% 
  tibble %>% 
  mutate(
    aim = case_when(
      direction == 'down' ~ as.integer(amount),
      direction == 'up' ~ as.integer(-1 * amount),
      T ~ as.integer(0.0)
    ),
    caim = cumsum(aim)
  ) %>% 
  mutate(
    horiz = ifelse(direction == 'forward', amount, 0),
    depth = ifelse(direction == 'forward', amount * caim, 0)
  )
d2 %>% select(horiz, depth) %>% colSums() %>% prod
