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
  print(c(stack, as.vector(crates[[stack]])))
}

# operation loop
for(step in data[11:length(data)]){
  
  # used for debugging
  # print(step)
  
  # initialize fresh vectors to temporarily hold changing crate stacks
  temp_to_move = vector()
  temp_to_stay = vector()
  
  # parse instructions (# of crates to move, src stack, dst stack)
  nums = stringr::str_extract_all(step, '\\d+')[[1]]
  nums = as.numeric(nums)
  
  # process for crates if whole stack is moved (none remain in src)
  if(nums[1] == length(crates[[nums[2]]])){
    temp_to_move = crates[[nums[2]]]
    temp_to_stay = vector()
  } else {
  # process for crates if some remain in src stack
    temp_to_move = crates[[nums[2]]][-c(1:(length(crates[[nums[2]]])-nums[1]))]
    temp_to_stay = crates[[nums[2]]][1:(length(crates[[nums[2]]])-nums[1])]
  }
  
  
  # append moved crates and assign new state to crate src
  crates[[nums[3]]] = append(crates[[nums[3]]], temp_to_move)
  crates[[nums[2]]] = temp_to_stay
  
  # print current crate stacks for debug
  
  # for(stack in 1:9){
  #   print(c(stack, as.vector(crates[[stack]])))
  # }
  
  # iterate once per button click, used for debugging
  
  # cont = readline(prompt="Press [enter] to continue")
  
}

# print out top crates of each stack
for(stack in 1:length(crates)){
  print(crates[[stack]][length(crates[[stack]])])
}