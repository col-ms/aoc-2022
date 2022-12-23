# loads the is_ordered function from external utilities script
source("day13/day13_utils.R")

data <- readLines("inputs/day13_input.txt") # read in data from input file
data <- data[data!=""]                      # remove empty lines

# initialize a vector of NAs with length equal to half the length of data
ord <- rep(NA, length(data)/2)

left <- data[seq(1, length(data), 2)] # extract odd-numbered elements of data
right <- data[seq(2, length(data), 2)] # extract even-numbered elements of data

# iterates over the elements of left and right
for(i in 1:length(left)){
  
  # convert the JSON string in left[i] and right[i] to individual lists
  l <- jsonlite::fromJSON(left[i], simplifyVector = FALSE)
  r <- jsonlite::fromJSON(right[i],  simplifyVector = FALSE)
  
  # calls the is_ordered function on l and r and stores result in ord[i]
  ord[i] <- is_ordered(l,r)
}

# print sum of the indices of ord that are TRUE
print(sum(which(ord)))