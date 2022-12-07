data <- readLines('input/day7.txt')
ncom <- length(data)

# Helper functions
obtain.filesize <- function(s) as.numeric(stringr::str_extract(s, '(\\d+)'))
remove.filesize <- function(s) trimws(gsub('(\\d+)', '', s))
new.dir.name <- function(s) trimws(gsub('\\$ cd ', '', gsub('^dir ', '', s)))


# Creating file tree ------------------------------------------------------
# First input is setting to root dir.
i <- 2
wd <- "/"
file.tree <- list("/" = list())
current.file.tree <- "file.tree[['/']]"
while(i < ncom){
  cat(sprintf("Current i (loop): %d\n", i))
  print(data[i])
  # If they `$ ls` then read commands until they `$ cd` or `$ ls` again.
  if(grepl('\\$ ls', data[i])){
    not.cd.or.ls <- TRUE
    while(not.cd.or.ls & i < ncom){ # Need to specify i < ncom here or gets stuck in loop!
      i <- i + 1
      if(grepl('^dir', data[i])){ # If its a new directory, then assign it as (another) list
        eval(parse(text = paste0(current.file.tree, '[[', 
                                 "'", new.dir.name(data[i]), "'", ']] <- list()')))
      }
      if(grepl('^(\\d+)', data[i])){ # If its a new file, then assign it as an element in wd
        eval(parse(text = paste0(current.file.tree, '[[',
                                 "'", remove.filesize(data[i]), "'", ']] <- ', obtain.filesize(data[i]))))
      }
      if(grepl('\\$ cd', data[i]) | grepl('\\$ ls', data[i])) not.cd.or.ls <- FALSE
    }
  }
  
  old.wd <- wd
  if(grepl('\\$ cd', data[i])){ # Updating working directory based on where they are.
    if(grepl('\\.\\.$', data[i])){ # If they've gone back up a directory, then follow it back.
      wd <- old.wd
      # If move UP a level, remove from current.file.tree
      current.file.tree <- gsub("\\[\\['\\w+'\\]\\]$",   # (i.e. the last item)
                                '', current.file.tree)
    }else{
      wd <- new.dir.name(data[i])
      current.file.tree <- paste0(current.file.tree, "[[", "'", wd, "'",  ']]')
    }
    i <- i + 1
  }
}

# Function to find total file size for each directory.
dir.size <- function(x){
  sum.out <- sum(unlist(x))
  if(any(sapply(x, class) == "list")){
    sum.nest <- lapply(x[which(sapply(x, class) == "list")], dir.size)
  }else{
    sum.nest <- 0
  }
  list(sum.out, sum.nest)
}


# Part 1 ------------------------------------------------------------------
size <- unlist(dir.size(file.tree))
sum(size[size<10e4]) #1778099

# Part 2 ------------------------------------------------------------------
target <- 30000000 - (70000000 - unname(size["/"])) # update space - unused space
sort(size[size>=target])[1]
# /.pcqjnl.lrrl.nwjggvr.bwmglvmt <- this directory
# 1623571 