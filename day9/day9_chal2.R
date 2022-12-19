# import helper functions
source("day9/day9_utils.R")

# read in data
data <- data_import()

# initialize storage for 10 rope segments
rope <- matrix(rep(c(0,0), 10), ncol = 2, byrow = TRUE)
tpos_list <- rope[10,]

# iterate through each move in the data
for(i in 1:nrow(data)){
  
  # determine the direction of the move
  move_dir <- get_hdir(data[i,1])
  
  # make the specified number of moves in the determined direction
  for(n in 1:data[i,2]){
    rope[1,] = rope[1,] + move_dir
    for(knot in 2:10){
      rope[knot,] = t_move(rope[knot-1,], rope[knot,])
    }
    tpos_list = rbind(tpos_list, rope[10,])
  }
}

# print the number of unique positions visited by the tail of the 10th rope
print(paste("Unique positions visited:", nrow(unique(tpos_list))))