dat <- readLines("input/day2.txt")
dat <- gsub("^Game\\s\\d?\\d?\\d\\:\\s", "", dat)

# Part 1 ------------------------------------------------------------------
sum(which(sapply(dat, function(s){
  turns <- trimws(el(strsplit(s, ';')))
  # stringr::str_extract_all(turns)
  num.blue <- as.numeric(gsub('[a-z]', '', stringr::str_extract(turns, "\\d?\\d?\\d?\\d\\sblue")))
  num.blue <- ifelse(is.na(num.blue), 0, num.blue)
  num.green <- as.numeric(gsub('[a-z]', '', stringr::str_extract(turns, "\\d?\\d?\\d?\\d\\sgreen")))
  num.green <- ifelse(is.na(num.green), 0, num.green)
  num.red <- as.numeric(gsub('[a-z]', '', stringr::str_extract(turns, "\\d?\\d?\\d?\\d\\sred")))
  num.red <- ifelse(is.na(num.red), 0, num.red)
  all(num.blue <= 14) & all(num.red <= 12) & all(num.green <= 13)
})))

# Part 2 ------------------------------------------------------------------

sum(sapply(dat, function(s){
  turns <- trimws(el(strsplit(s, ';')))
  # stringr::str_extract_all(turns)
  num.blue <- as.numeric(gsub('[a-z]', '', stringr::str_extract(turns, "\\d?\\d?\\d?\\d\\sblue")))
  num.blue <- ifelse(is.na(num.blue), 0, num.blue)
  num.green <- as.numeric(gsub('[a-z]', '', stringr::str_extract(turns, "\\d?\\d?\\d?\\d\\sgreen")))
  num.green <- ifelse(is.na(num.green), 0, num.green)
  num.red <- as.numeric(gsub('[a-z]', '', stringr::str_extract(turns, "\\d?\\d?\\d?\\d\\sred")))
  num.red <- ifelse(is.na(num.red), 0, num.red)
  max(num.blue) * max(num.red) * max(num.green)
}))
