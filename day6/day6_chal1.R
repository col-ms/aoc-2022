# read in data and split string into characters for parsing
data <- readLines("inputs/day6_input.txt") |> 
  strsplit(split = '') |> 
  unlist()

# search through string until last 4 observed are all unique
for(i in 5:length(data)){
  last4 = data[(i-4):(i-1)]
  if(length(unique(last4)) == 4){
    print(i-1)
    break()
  }
}