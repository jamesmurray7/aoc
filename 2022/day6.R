data <- readLines('input/day6.txt')

# A marker is four characters that are all different
# In example seventh character is needed, as PRECEDING four characters are all different.
ex <- el(strsplit("mjqjpqmgbljsphdztnvjfqwrcgsmlb",''))

for(i in 4:length(ex)){
  print(ex[(i-3):i])
  print(unique(ex[(i-3):i]))
  print(length(unique(ex[(i-3):i])))
  print(i) # so when i = 7 first occurrence of this...
}

data <- el(strsplit(data, ''))
for(i in 4:length(data)){
  crit <- length(unique(data[(i-3):i]))
  if(crit > 3) stop(i)  #1625
}

for(i in 14:length(data)){
  crit <- length(unique(data[(i-13):i]))
  if(crit > 13) stop(i) #2250
}
