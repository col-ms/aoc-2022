data <- readLines("inputs/day4_input.txt")

# solution using vectorized function

does_overlap <- function(zones){
  
  znums = as.numeric(stringr::str_extract_all(zones, "[0-9]{1,}")[[1]])
  
  return(ifelse(
    (znums[1] >= znums[3]) && (znums[2] <= znums[4]) ||
    (znums[3] >= znums[1]) && (znums[4] <= znums[2]),
    TRUE, FALSE))
  
}

sum(unlist(lapply(data, does_overlap)))

# alternate solution using for loop

total = 0

for(i in data){
  zones = strsplit(i, ',')[[1]]
  
  x = as.numeric(strsplit(zones[1], '-')[[1]])
  y = as.numeric(strsplit(zones[2], '-')[[1]])
  
  if(x[1] <= y[1] && x[2] >= y[2]){
    total = total + 1
  } else if(y[1] <= x[1] && y[2] >= x[2]){
    total = total + 1
  }
}