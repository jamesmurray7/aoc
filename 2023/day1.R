dat <- readLines("input/day1.txt")[-1001]
splits <- sapply(dat, strsplit, '')

# Part 1 ------------------------------------------------------------------
first.last <- lapply(splits, function(s){
  ww <- grep('[0-9]', s)
  as.numeric(paste(s[c(min(ww), max(ww))], collapse=''))
})
Reduce('+', first.last) # 54940

# Part 2 ------------------------------------------------------------------
nums <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

sum(sapply(sapply(splits, paste, collapse=''), function(s){
  out <- c()
  for(i in 1:nchar(s)){
    if(grepl("[0-9]", substr(s, i, i))) out <- c(out, substr(s,i,i))
    if(substr(s, i, i + 2)%in%nums) out <- c(out, substr(s, i, i + 2)) #one, two, six
    if(substr(s, i, i + 3)%in%nums) out <- c(out, substr(s, i, i + 3)) #four, five, nine
    if(substr(s, i, i + 4)%in%nums) out <- c(out, substr(s, i, i + 4)) # three, seven, eight
  }
  out <- out[!is.na(out)]
  out <- sapply(out, function(x){
    if(grepl("[0-9]", x))
      return(x)
    else
      return(which(nums==x))
  })
  as.numeric(paste(out[c(1, length(out))], collapse = ''))
  # out[c(1, length(out))]
}))
# 54208
