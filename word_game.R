words <- read.table('words_alpha.txt',header = T,stringsAsFactors = F)

library(stringr)
library(gtools)
library(dplyr)
library(tidyr)

word_game_function <- function(input, min){
  x <- unlist(strsplit(input, split = "")) # Splits the string into single characters
  l <- length(x) # determine amount of characters
  merge <- NULL # initialise
  
  for(i in min:l) {
    if(l>7){print('Word is too long!'); break}
    # The game has a maximum of 7 characters at a time, also stops long words
    # from taking up computation time
    perm <- as.data.frame(permutations(n=l, r=i, v=x, set=F)) # find every permutation at each length
    merge <- rbind(merge, unique(unite(perm, a, everything(), sep = ""))) # Store all permutations
  }
  return(intersect(words, merge)) # find words that exist in permutations and dictionary, return result
}

word_game_function('dveeni', 4)

