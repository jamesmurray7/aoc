df <- as.data.frame(matrix(readLines('data/day6.txt'), nc = 1))
# part a
df$group <- cumsum(df$V1=='')
df <- df[df$V1!='',]
sum(tapply(df$V1, df$group, function(x){
  length(unique(strsplit(paste0(x, collapse = ''), '')[[1]]))
})) # 6351

# part b
test <- df[df$group==0,]
length(test$V1)

sum(tapply(df$V1, df$group, function(x){
  l <- length(x)
  tab <- table(strsplit(paste0(x, collapse = ''), '')[[1]])
  length(unique(names(which(tab == l))))
})) # 3143