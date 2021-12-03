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