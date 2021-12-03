#' ######
#' Work-in-progress script for solutions to AoC 2020
#' #####
rm(list=ls())
setwd('c:/Users/James/Dropbox/R work/AoC/2020')
library(tidyverse)

# Day 1 -------------------------------------------------------------------
d <- read.table('./data/day1.txt')
# part a)
d <- c(d$V1)
dod <- outer(d, d, FUN = '+')
sum.to.2020 <- which(dod == 2020, arr.ind = T)[1,]
prod(d[unique(sum.to.2020)]) # answer 

# part b)
sum3.to.2020 <- which(outer(dod, d, '+')==2020, arr.ind = T)[1,]
prod(d[sum3.to.2020])

# Day 2 -------------------------------------------------------------------
rm(list=ls())
d <- tibble(read.table('./data/day2.txt'))
names(d) <- c('rule', 'letter', 'password')

# part a)
d2 <- d %>% 
  mutate(
    min = as.integer(str_extract(rule, '^\\d?\\d')),
    max = as.integer(str_extract(rule, '\\-.*$') %>% str_remove(., '\\-')),
    letter = str_remove_all(letter, '\\:')
  ) %>% rowwise %>% 
  mutate(
    letter.count = map_dbl(letter, ~ str_count(password, .x))
  ) %>% ungroup

valid.pw <- d2 %>% rowwise %>% 
  mutate(is.valid = between(letter.count, min, max)) %>% ungroup

valid.pw %>% count(is.valid) # 416, correct.

# part b)
rm(d2, valid.pw)
d2 <- d %>% 
  rowwise %>% 
  mutate(
    ind1 = as.integer(str_extract(rule, '^\\d?\\d')),
    ind2 = as.integer(str_extract(rule, '\\-.*$') %>% str_remove(., '\\-')),
    letter = str_remove(letter, '\\:'),
    ind1.char = map_chr(ind1, ~substr(password, .x, .x)),
    ind2.char = map_chr(ind2, ~substr(password, .x, .x)),
    ind1.T = ind1.char == letter,
    ind2.T = ind2.char == letter
  ) %>% 
  ungroup

d2$check <- xor(d2$ind1.T, d2$ind2.T)
sum(d2$check) # 688, correct.


# Day 3 -------------------------------------------------------------------
rm(list=ls())
d <- as.matrix(read.table('./data/day3.txt', comment.char = '@'))
d <- do.call(rbind, str_split(d[, 1], ''))

# "The same pattern repeats to the right many times:" 
d100 <- replicate(100, d, simplify = F)
d <- do.call(cbind, d100)

# part a)
run <- c()
this.row <- 1; this.col <- 1
this.pos <- d[this.row, this.col]
for(i in 1:(nrow(d) - 1)){
   next.row <- this.row + 1
   next.col <- this.col + 3
   next.pos <- d[next.row, next.col]
   isTREE <- ifelse(next.pos=='#', 'TREE', 'SAFE')
   run[i] <- isTREE
   message('Old position: d[', this.row, ', ', this.col, ']. New position: d[', next.row, ', ', next.col,
         ']\nWith value ', isTREE)
   this.pos <- next.pos; this.row <- next.row; this.col <- next.col
}
length(which(run == 'TREE')) # 198, correct

# part b)
# probably best to define a function
rightxdowny <- function(x, y){
  message('Going ACROSS by ', x, 'and DOWN by ', y, 'at each iteration.')
  run <- c()
  this.row <- 1; this.col <- 1
  this.pos <- d[this.row, this.col]
  for(i in 1:nrow(d)){
    next.col <- this.col + x
    next.row <- this.row + y
    next.pos <- d[next.row, next.col]
    isTREE <- ifelse(next.pos=='#', 'TREE', 'SAFE')
    run[i] <- isTREE
    message('Old position: d[', this.row, ', ', this.col, ']. New position: d[', next.row, ', ', next.col,
            ']\nWith value ', isTREE)
    this.pos <- next.pos; this.row <- next.row; this.col <- next.col
    if(this.row + y > nrow(d)) break;
  }
  as.numeric(length(which(run == 'TREE')))
}

r3d1 <- rightxdowny(3, 1) # same as part a
r1d1 <- rightxdowny(1, 1) 
r5d1 <- rightxdowny(5, 1)
r7d1 <- rightxdowny(7, 1)
r1d2 <- rightxdowny(1, 2)
r3d1 * r1d1 * r5d1 * r7d1 * r1d2 # 5140884672. Correct


# Day 4 -------------------------------------------------------------------
rm(list=ls())
d <- tibble(read.table('./data/day4.txt', fill = T, sep = '@', as.is = T, blank.lines.skip = F, comment.char = '~'))
d$flag <- ifelse(d$V1 == "", 1, 0)
d$id <- cumsum(d$flag) + 1

# Part a)
# Transform
d2 <- d %>% 
  filter(V1 != "") %>% 
  select(-flag) %>% 
  group_by(id) %>% 
  mutate(r = row_number()) %>% 
  pivot_wider(., id_cols = id, values_from = `V1`, names_from = r, names_prefix = 'item') %>% 
  mutate(across(item1:item6, ~ ifelse(is.na(.x), '', .x)),
    passport = sort(trimws(paste(item1, item2, item3, item4, item5, item6)))) %>% ungroup %>% select(id, passport)

# Check all necessary items
to.check <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")#, "cid")  # For this exercise, missing CID is fine.
checks <- sapply(d2$id, function(i){ # There is probably a much, MUCH nicer way to do this !!!
  pass.check <- c()
  for(j in seq_along(to.check)){
    pass.check[j] <- !is.na(as.character(str_match(d2[d2$id == i, 'passport'], to.check[j])))
  }
  pass.check
})

sum(apply(checks, 2, all)) # 200, correct.

# part b)
rm(to.check, checks)

all.checks <- d2 %>% 
  mutate(
    # Birth year (1920 - 2002)
    byr = str_extract(passport, 'byr:*\\S+'),
    byr.check = between(parse_number(byr), 1920, 2002),
    # Issue year (2010 - 2020)
    iyr = str_extract(passport, 'iyr:*\\S+'),
    iyr.check = between(parse_number(iyr), 2010, 2020),
    # Expiration year (2020 - 2030)
    eyr = str_extract(passport, 'eyr:*\\S+'),
    eyr.check = between(parse_number(eyr), 2020, 2030),
    # Height (if cm 150 - 193; if in 59 - 76)
    hgt = str_extract(passport, 'hgt:*\\S+'),
    hgt.check = case_when(
      str_detect(hgt, 'cm') & between(parse_number(hgt), 150, 193) ~ T,
      str_detect(hgt, 'in') & between(parse_number(hgt), 59, 76) ~ T,
      T ~ F
    ),
    # Hair colour, #<6 alphanumeric chars>
    hcl = str_extract(passport, 'hcl:*\\S+'),
    hcl.check = case_when(
      str_detect(str_remove(hcl, 'hcl\\:'), '\\#') & nchar(str_remove(hcl, 'hcl\\:\\#')) == 6 ~ T,
      T  ~ F
    ),
    # Eye colour: exactly one of: amb blu brn gry grn hzl oth.
    ecl = str_extract(passport, 'ecl:*\\S+'),
    ecl.check = case_when(
      str_detect(ecl, 'amb') | str_detect(ecl, 'blu') | str_detect(ecl, 'brn') | 
      str_detect(ecl, 'gry') | str_detect(ecl, 'grn') | str_detect(ecl, 'hzl') | str_detect(ecl, 'oth') ~ T,
      T ~ F
    ),
    # Passport ID: a nine-digit number, including leading zeroes.
    pid = str_extract(passport, 'pid:*\\S+'),
    pid.check = case_when(
      nchar(str_remove(pid, 'pid\\:')) == 9 & !str_detect(str_remove(pid, 'pid\\:'), '[[a-z]]') ~ T,
      T ~ F
    )
  )

numvalids <- all.checks %>% select(ends_with('check')) %>% rowSums 
length(which(numvalids == 7))









