# read in data from text file and split into individual characters
data <- readLines("inputs/day8_input.txt") |>
  strsplit(split = '')

# convert character data to numeric values and store in matrix
data = matrix(as.numeric(unlist(data)), nrow = length(data), byrow = TRUE)

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