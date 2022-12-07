data <- readLines("inputs/day4_input.txt")

total = 0

for(i in data){
  zones = strsplit(i, ',')[[1]]
  
  x = as.numeric(strsplit(zones[1], '-')[[1]])
  y = as.numeric(strsplit(zones[2], '-')[[1]])
  
  if(x[1] <= y[1] && y[1] <= x[2]){
    total = total + 1
  } else if(x[1] <= y[2] && y[2] <= x[2]){
    total = total + 1
  } else if(y[1] <= x[1] && x[1] <= y[2]){
    total = total + 1
  }
}