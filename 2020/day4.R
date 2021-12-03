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