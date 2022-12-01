data <- do.call(rbind, as.list(readLines('input/day1.txt')))
data <- as.numeric(data)
data <- as.data.frame(data);names(data)='cal'
library(dplyr)

# part 1
data %>% 
  mutate(inc = ifelse(is.na(cal), 1 , 0),
         elfno = cumsum(inc)) %>% 
  select(-inc) %>% 
  group_by(elfno) %>% 
  filter(!is.na(cal)) %>% 
  summarise(sum = sum(cal)) %>% 
  arrange(-sum) %>% head(1)

# part 2 (same but keep 3)
data %>% 
  mutate(inc = ifelse(is.na(cal), 1 , 0),
         elfno = cumsum(inc)) %>% 
  select(-inc) %>% 
  group_by(elfno) %>% 
  filter(!is.na(cal)) %>% 
  summarise(sum = sum(cal)) %>% 
  arrange(-sum) %>% head(3) %>% colSums
