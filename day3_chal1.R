library(dplyr)

data <- readLines("day3_input.txt")

comp_split <- function(items){
  
  comp1 = substr(items, 1, nchar(items)/2)
  comp2 = substr(items, (nchar(items)/2 + 1), nchar(items))
  
  return(c(comp1, comp2))
}

comp_items = comp_split(data)

c1 <- comp_items[1:(length(comp_items)/2)]
c2 <- comp_items[(length(comp_items)/2 + 1):(length(comp_items))]



find_dupes <- function(c1, c2){
  
  get_chars <- function(str){return(strsplit(str, '')[[1]])}
  
  return(intersect(get_chars(c1), get_chars(c2)))
}

commons <- vector()

for(i in 1:length(c1)){
  commons = append(commons, find_dupes(c1[i], c2[i]))
}

my_letters <- c(letters[1:26], LETTERS[1:26])

prs <- match(commons, my_letters)

sum(prs)