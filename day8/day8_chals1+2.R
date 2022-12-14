#### Data Ingestion

# read in data from text file and split into individual characters
data <- readLines("inputs/day8_input.txt") |>
  strsplit(split = '')

# convert character data to numeric values and store in matrix
data = matrix(as.numeric(unlist(data)), nrow = length(data), byrow = TRUE)

#### PART 1

# initialize variable for visible trees
visible <- (2*nrow(data) + 2*ncol(data)) - 4

# iterate over elements in matrix
for(row in 2:(nrow(data)-1)){

  for(col in 2:(ncol(data)-1)){
    
    # store current element value
    cur_tree = data[row,col]
    
    # check if current element has a higher value than any adjacent elements
    if(
      max(data[row,1:(col-1)]) < cur_tree ||          # check left
      max(data[row,(col+1):ncol(data)]) < cur_tree || # check right
      max(data[1:(row-1),col]) < cur_tree ||          # check above
      max(data[(row+1):nrow(data),col]) < cur_tree){  # check below
        
        # increment visible tree count
        visible = visible + 1
      
    }  
  }
}

# print number of visible trees
print(paste("Part 1: Number of visible trees:", visible))

#### PART 2

# define function to calculate visible trees in a given direction
getVD <- function(my_tree, trees_vec){
  
  # if only one tree in vector, it is automatically visible
  if(length(trees_vec) == 1){
    return(1)
  } 
  
  # if all trees in vector are shorter than current tree, all are visible
  else if(all(trees_vec < my_tree)){
    return(length(trees_vec))
  }
  
  # initialize visible tree count
  vdist = 1
  
  # iterate over trees in vector
  for(i in trees_vec){
    
    # increment count if tree is shorter than current tree
    if(i < my_tree){
      vdist = vdist + 1
    } 
    
    # return count if tree is taller or equal to current tree
    else if(i >= my_tree){
      return(vdist)
    }
  }
  
  # return final count
  return(vdist)
}

# initialize variable to track maximum scenic score
max_scenic_score <- 0

# iterate over elements in matrix
for(row in 2:(nrow(data)-1)){
  
  for(col in 2:(ncol(data)-1)){
    
    # store current element value
    cur_tree = data[row,col]
    
    # store trees in each cardinal direction from current position
    trees_l = rev(data[row,1:(col-1)])
    trees_r = data[row,(col+1):ncol(data)]
    trees_u = rev(data[1:(row-1),col])
    trees_d = data[(row+1):nrow(data),col]
    
    # calculate scenic score as product of # of visible trees in each direction
    scenic_score = prod(getVD(cur_tree,trees_l),
                        getVD(cur_tree,trees_r),
                        getVD(cur_tree,trees_u),
                        getVD(cur_tree,trees_d))
    
    # update maximum scenic score if necessary
    max_scenic_score = max(max_scenic_score, scenic_score)
  }
}

# print maximum scenic score
print(paste("Part 2: Maximum scenic score:", max_scenic_score))