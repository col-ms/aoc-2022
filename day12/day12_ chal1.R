data <- readLines("inputs/day12_input.txt") |>
  strsplit(split = '') |>
  unlist() |>
  matrix(nrow = 41, byrow = TRUE)

# data <- "SabqponmabcryxxlaccszExkacctuvwjabdefghi" |>
#   strsplit(split = '') |>
#   unlist() |>
#   matrix(nrow = 5, byrow = T)

start <- which(data == "S", arr.ind = TRUE)
end <- which(data == "E", arr.ind = TRUE)

c2n <- function(c){
  if(c == "E"){return(26)}
  if(c == "S"){return(1)}
  return(which(letters == c))
}

bfs <- function(data, src, end){

  R = nrow(data) # find max row number
  C = ncol(data) # find max col number
  
  sr = src[1] # store start row
  sc = src[2] # store start col
  
  er = end[1] # store end row
  ed = end[2] # store end col
  
  rq = vector() # initialize row queue
  cq = vector() # initialize col queue
  
  rq = c(rq, sr) # append source row to row queue
  cq = c(cq, sc) # append source col to col queue
  
  move_cnt = 0
  nodes_left_in_layer = 1
  nodes_in_next_layer = 0
  found_end = FALSE
  
  visited = matrix(FALSE, nrow = R, ncol = C) # initialize matrix 
                                              # to track visited cells
  visited[sr,sc] = TRUE # mark start location as visited 
  
  while(length(rq)!=0){
  
    r = rq[1]   # assign first element in row queue as current row
    rq = rq[-1] # remove first element in row queue
    
    c = cq[1]   # assign first element in col queue as current col
    cq = cq[-1] # remove first element in col queue
    
    if(data[r,c] == 'E'){
      found_end = TRUE
      break
    }
    
    print(paste("r =", r))
    print(paste("c =", c))
    
    dr = c(-1, 1, 0, 0)
    dc = c(0, 0, 1, -1)
      
    for(i in 1:4){
      
      rr = r + dr[i]
      cc = c + dc[i]
      
      print(paste("checking: ", rr, ',', cc))
      
      if(rr < 1 || cc < 1){
        print("skipping due to < 1 condition")
        next
      }
      if(rr > R || cc > C){
        print("skipping due to > R|C condition")
        next
      }
      
      if(visited[rr,cc]){
        print("skipping due to already visited")
        next
      }
      if((c2n(data[rr,cc]) - c2n(data[r,c])) > 1){
        print("skipping due to height condition")
        next
      }
      
      rq = c(rq, rr)
      cq = c(cq, cc)
      visited[rr,cc] = TRUE
      nodes_in_next_layer = nodes_in_next_layer + 1
    }
    
    nodes_left_in_layer = nodes_left_in_layer - 1
    
    if(nodes_left_in_layer == 0){
      nodes_left_in_layer = nodes_in_next_layer
      nodes_in_next_layer = 0
      move_cnt = move_cnt + 1
    }
  }
  if(found_end){
    return(move_cnt)
  }
  return("Err: end not found")
}

print(bfs(data, start, end))
