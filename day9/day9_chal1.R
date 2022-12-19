# import helper functions
source("day9/day9_utils.R")

# read in data
data <- data_import()

# initialize starting positions for head and tail of rope
hpos <- c(0,0)
tpos <- c(0,0)
tpos_list <- tpos

# iterate through each move in the data
for(i in 1:nrow(data)){
  
  # determine the direction of the move
  move_dir <- get_hdir(data[i,1])

  # make the specified number of moves in determined direction
  for(n in 1:data[i,2]){
    hpos = hpos + move_dir
    tpos = t_move(hpos, tpos)
    tpos_list = rbind(tpos_list, tpos)
  }

}

# print the number of unique positions visited by the tail of the rope
print(paste("Unique positions visited:", nrow(unique(tpos_list))))