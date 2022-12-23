# Read in input data, split in characters, and store in a matrix
data <- readLines("inputs/day12_input.txt") |>
  strsplit(split = '') |>
  unlist() |>
  matrix(nrow = 41, byrow = TRUE)

# define a function to covert letters to numbers
c2n <- function(c){
  if(c == "E"){return(26)}
  if(c == "S"){return(1)}
  return(which(letters == c))
}

# define a function to perform breadth-first search (bfs) to find the shortest
# path between the start and end locations in the matrix
bfs <- function(data, part = 1, verbose = FALSE){

  # sets target and starting cell based on what part to solve for
  if(part == 1){
    src = which(data == "S", arr.ind = TRUE)
    target = "E"
  } 
  else if(part == 2){
    src = which(data == "E", arr.ind = TRUE)
    target = "a"
  }
  else { # ensures input sanitation from function call
    return("Invalid part argument. Expected values of 1 or 2.")
  }
  
  # find the number of rows and columns in the matrix
  R = nrow(data)
  C = ncol(data)
  
  # store the start location's row and column
  sr = src[1]
  sc = src[2]
  
  # store the end location's row and column
  #er = end[1]
  #ed = end[2]
  
  # initialize vectors to store the rows and columns of nodes in the queue
  rq = vector()
  cq = vector()
  
  # add the start location's row and column to the queue
  rq = c(rq, sr)
  cq = c(cq, sc)
  
  # initialize variables to track the number of moves made, the number of nodes
  # left in the current layer, the number of nodes in the next layer, and
  # whether the end location has been found
  move_cnt = 0
  nodes_left_in_layer = 1
  nodes_in_next_layer = 0
  found_end = FALSE
  
  # initialize a matrix to track which cells have been visited
  visited = matrix(FALSE, nrow = R, ncol = C)
  
  # mark the start location as visited 
  visited[sr,sc] = TRUE
  
  # while there are nodes remaining in the queue...
  while(length(rq)!=0){
  
    # remove the first element in the row queue and assign it as the current row
    r = rq[1]
    rq = rq[-1]
    
    # remove the first element in the col queue and assign it as the current col
    c = cq[1]
    cq = cq[-1]
    
    # if the current cell is the end location, set the found_end flag to TRUE
    # and break out of the loop
    if(data[r,c] == target){
      found_end = TRUE
      break
    }
    
    # for logging
    if(verbose){
      print(paste("r =", r))
      print(paste("c =", c))
    }
      
    # define arrays of row and column deltas to move to the cells directly
    # above, below, to the right, and to the left of the current cell
    dr = c(-1, 1, 0, 0)
    dc = c(0, 0, 1, -1)
    
    # for each of the 4 cells surrounding the current cell  
    for(i in 1:4){
      
      # calculate the row and column indices of the neighbouring cells
      rr = r + dr[i]
      cc = c + dc[i]
      
      if(verbose){print(paste("checking: ", rr, ',', cc))}
      
      # the neighbouring cell is out of bounds, skip it
      if(rr < 1 || cc < 1){
        if(verbose){print("skipping due to < 1 condition")}
        next
      }
      if(rr > R || cc > C){
        if(verbose){print("skipping due to > R|C condition")}
        next
      }
      
      # if the neighbouring cell has already been visited, skip it
      if(visited[rr,cc]){
        if(verbose){print("skipping due to already visited")}
        next
      }
      
      # if the height difference between the current cell and the neighbouring
      # cell is larger than 1, skip it (note: the neighbouring cell can be
      # lower than the current cell with no restrictions)
      if((c2n(data[rr,cc]) - c2n(data[r,c])) > 1 && part == 1){
        if(verbose){print("skipping due to part 1 height condition")}
        next
      }
      
      # same as above rule, but for reverse travel (can't step down more than
      # a 1 height difference)
      if((c2n(data[r,c]) - c2n(data[rr,cc])) > 1 && part == 2){
        if(verbose){print("skipping due to part 2 height condition")}
        next
      }
      # add the neighbouring cell's row and column to the queues
      rq = c(rq, rr)
      cq = c(cq, cc)
      
      # mark the neighbouring cell as visited
      visited[rr,cc] = TRUE
      
      # increment the count of nodes in the next layer
      nodes_in_next_layer = nodes_in_next_layer + 1
    }
    
    # decrement the count of nodes left in the current layer
    nodes_left_in_layer = nodes_left_in_layer - 1
    
    # if there are no more nodes left in the current layer
    if(nodes_left_in_layer == 0){
      
      # set the count of nodes left in current layer to the
      # count of nodes in the next layer
      nodes_left_in_layer = nodes_in_next_layer
      
      # reset the count of nodes in the next layer to 0
      nodes_in_next_layer = 0
      
      # increment the move counter
      move_cnt = move_cnt + 1
    }
  }
  # the end location was found, return the number of moves it took to get there
  if(found_end){
    return(move_cnt)
  }
  
  # if no end was found, return an error message
  return("Err: end not found")
}


print(paste("Part 1:", bfs(data, part = 1, verbose = FALSE)))
print(paste("Part 2:", bfs(data, part = 2, verbose = FALSE)))

library(microbenchmark)
microbenchmark(bfs(data, part = 1))
microbenchmark(bfs(data, part = 2))
