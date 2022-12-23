# read in data from input file
data <- readLines("inputs/day14_input.txt")

# split each string by " -> " and then "," resulting in a list of integers
data <- lapply(strsplit(data, " -> "),
               function(x) apply(do.call(rbind, strsplit(x,',')),2,as.integer))

# initialize matrix of 0s to store cave structure information
cave <- matrix(0, ncol = 600, nrow = 200)

# iterate over elements of data
for(i in seq_along(data)){
  for(j in 2:nrow(data[[i]])){
    cave[data[[i]][j-1,2]:data[[i]][j,2],     # update value in cave to 1
         data[[i]][j-1,1]:data[[i]][j,1]] = 1 # corresponding to coords in data
  }
}

sand <- 0   # initialize sand grain counter
lx <- 0     # vector to store x pos history
ly <- 500   # vector to store y pos history
continue = TRUE

# begins sand fill loop
while(continue){
  
  # set current x and y positions to last value in position history vectors
  x = lx[length(lx)]
  y = ly[length(ly)]
  
  # begins loop that continues until 'stop' condition is met
  repeat{

    # if sand has reached the bottom of the cave, 
    # stop loop
    if(x>=nrow(cave)){
      continue = FALSE
      break
    }
    
    # if there is space for the sand to flow directly downwards,
    # add current pos to history and increment x pos
    if(cave[x+1,y]==0){
      lx = c(lx,x)
      ly = c(ly,y)
      x = x + 1
    } 
    
    # if there is space for sand to flow diagonally down and left,
    # add current pos to history and update x and y pos
    else if(cave[x+1,y-1]==0){
      lx = c(lx,x)
      ly = c(ly,y)
      x = x + 1
      y = y - 1
    } 
    
    # if there is space for sand to flow diagonally down and right,
    # add current pos to history and update x and y pos
    else if(cave[x+1,y+1]==0){
      lx = c(lx,x)
      ly = c(ly,y)
      x = x + 1
      y = y + 1
    } 
    
    else {                 # if the sand has nowhere to move
      if(cave[x,y]!=2){    # and the current location isn't already sand,
        cave[x,y] = 2      # set current location to be sand,
        sand = sand + 1    # update sand count
      # cat(sand, "\r")    # print current sand count
      }
      lx = lx[-length(lx)] # remove last visited position
      ly = ly[-length(ly)] # from history vectors (now occupied by sand)
      break                # break out of repeat loop
    }
  }
}