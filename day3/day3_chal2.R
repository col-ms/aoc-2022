data <- readLines("inputs/day3_input.txt")

find_dupes <- function(e1, e2, e3){
  
  get_chars <- function(str){return(strsplit(str, '')[[1]])}
  
  return(intersect(intersect(get_chars(e1), get_chars(e2)), get_chars(e3)))
}

badges = vector()

i = 1

while(i < 300){
  badges = append(badges, find_dupes(data[i], data[i+1], data[i+2]))
  i = i + 3
}

sum(match(badges, c(letters[1:26], LETTERS[1:26])))