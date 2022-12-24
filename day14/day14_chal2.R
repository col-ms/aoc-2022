# read in data from input file
data <- readLines("inputs/day14_input.txt")

# split each string by " -> " then by "," resulting in list of ints
data <- lapply(strsplit(data, " -> "),
               function(x) apply(do.call(rbind, strsplit(x,',')),2,as.integer))

# initialize matrix of 0s to store cave info
cave <- matrix(0, ncol = 600, nrow = 200)

# iterate over data, updating cave structure according to positions in data
for(i in seq_along(data)){
  for(j in 2:nrow(data[[i]])){
    cave[data[[i]][j-1,2]:data[[i]][j,2],
         data[[i]][j-1,1]:data[[i]][j,1]] = 1
  }
}

# set cave "floor" at 2 rows below lowest rock formation (row w/ 1 present)
cave[max(which(apply(cave, 1, sum)!=0))+2,] = 1

sand <- 0       # sand grain counter
lx <- 0         # x position history vector
ly <- 500       # y position history vector
continue = TRUE # flag used for while loop condition

# while continue flag is set to TRUE
while(continue){
  
  # set current x and y positions to last value in pos history vectors
  x = lx[length(lx)] 
  y = ly[length(ly)]
  
  # if the sand could potentially extend past the 
  # current cave walls, extend them
  if(y+3 == ncol(cave)){
    cave = cbind(cave, cave[,1])
  }
  
  # begin loop to continue until break condition is met
  repeat{

    # if sand extends past cave floor, stop loop
    if(x>=nrow(cave)){stop(sand)}
    
    # if sand can move down, do so and update history vectors
    if(cave[x+1,y]==0){
      lx = c(lx,x)
      ly = c(ly,y)
      x = x + 1
    } 
    
    # if sand can move down and left, do so and update history vectors
    else if(cave[x+1,y-1]==0){
      lx = c(lx,x)
      ly = c(ly,y)
      x = x + 1
      y = y - 1
    } 
    
    # if sand can move down and right, do so and update history vectors
    else if(cave[x+1,y+1]==0){
      lx = c(lx,x)
      ly = c(ly,y)
      x = x + 1
      y = y + 1
    } 
    
    else {                 # if sand has no next valid location
      if(x == 0){          # and current x is top of the cave (x = 0)
        sand = sand + 1    # increment sand
        continue = FALSE   # stop loop
        break              # exit loop
      }
      if(cave[x,y]!=2){    # if current pos is not sand
        cave[x,y] = 2      # set current pos to sand
      # cat(sand, "\r")
        sand = sand + 1    # increment sand
      }
      lx = lx[-length(lx)] # remove last element
      ly = ly[-length(ly)] # of history vectors
      break                # and break out of repeat loop
    }
  }
}