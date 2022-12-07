# read in input data
data <- readLines("inputs/day5_input.txt")

# temporary matrix storage for initial crate config
tmp = matrix(nrow = 9, ncol = 8)

# parsing text info for initial crate config
for(i in 1:8){
  row = gsub("   ", NA, stringr::str_extract_all(data[i], ".{3,4}")[[1]]) |> 
        trimws()
  tmp[,9-i] = row
}

# store crate info in list
crates = list()

for(stack in 1:9){
  crates[[stack]] = na.omit(tmp[stack,])
}

# operation loop
for(step in data[11:length(data)]){
  
  # parse instructions (# of crates to move, src stack, dst stack)
  nums = stringr::str_extract_all(step, '\\d+')[[1]]
  nums = as.numeric(nums)
  
  # move the last (top) crate from src to dst, remove crate from src
  # repeat for # of crates to move per instruction
  for(n in 1:nums[1]){
    crates[[nums[3]]] = append(crates[[nums[3]]], 
                               crates[[nums[2]]][length(crates[[nums[2]]])])
    crates[[nums[2]]] = crates[[nums[2]]][-length(crates[[nums[2]]])]
  }
  
}

# print out top crates of each stack
for(stack in 1:length(crates)){
  print(crates[[stack]][length(crates[[stack]])])
}
