# Reads in data from a file and extracts all integers.
# Converts the resulting character vector to a matrix with 4 columns.
data <- readLines("inputs/day15_input.txt") |>
  stringr::str_extract_all(pattern = "-*\\d+") |>
  unlist() |>
  as.numeric() |>
  matrix(ncol = 4, byrow = TRUE)

# Defines a function to calculate the Manhattan distance between two points.
manh <- function(x1,x2,y1,y2){
  return(abs(x1-x2)+abs(y1-y2))
}

# Calculates the Manhattan distance between each pair of points (sensor and
# beacon) in the data and appends the resulting vector to the matrix.
data <- cbind(data, manh(data[,1],data[,3],data[,2],data[,4]))

# Defines a function to check if a point is within the range of any sensors
# in the data.
not_covered <- function(pt, data, val.y = 4000000){
  
  found = FALSE
  
  #If pt is outside the y-range of interest, return false.
  if(any(pt > val.y) || any(pt < 0)) return(FALSE)
  
  # Calculate the Manhattan distance between pt and each sensors location.
  sdist = manh(pt[1],data[,1],pt[2],data[,2])
  
  # If the distance between pt and sensor is greater than the sensor's range
  # for all sensors, return true.
  if(all(sdist>data[,5])){
    print("Found!")
    found = TRUE
  }
  return(found)
}

# Defines a function to check the perimeter of a sensor's range, given the
# location and range of a sensor.
check_peri <- function(data,i){
  
  # Calculates the coordinates of the bottom, top, left, and right points
  # of the sensor range.
  sx = data[i,1]
  sy = data[i,2]
  d = data[i,5]
  
  b = c(sx,sy + d) + c(0, 1)
  t = c(sx,sy - d) + c(0,-1)
  r = c(sx + d,sy) + c(1, 0)
  l = c(sx - d,sy) + c(-1,0)
  
  # Directional vectors to navigate through coordinates diagonally.
  dr = c(1,  1) # Down and right
  dl = c(-1, 1) # Down and left
  ur = c(1, -1) # Up and right
  ul = c(-1,-1) # Up and left
  
  # Initialize the search at the bottom point of the range perimeter.
  pt = b
  found = FALSE
  
  # While pt is not the rightmost point...
  while(all(pt!=r)){
    # Check if point is within any sensor's range.
    found = not_covered(pt,data)
    # If pt is not covered, return pt.
    if(found){
      print(pt)
      return(pt)
    }
    # If not, move pt diagonally up and to the right.
    pt = pt + ur
  }
  
  # While pt is not the topmost point...(repeat above steps with new direction)
  while(all(pt!=t)){
    found = not_covered(pt,data)
    if(found){
      print(pt)
      return(pt)
    }
    pt = pt + ul
  }
  
  # Same logic as above, now moving down and to the left until leftmost point.
  while(all(pt!=l)){
    found = not_covered(pt,data)
    if(found){
      print(pt)
      return(pt)
    }
    pt = pt + dl
  }
  
  # Last direction check, down and to the right. Same logic applies.
  while(all(pt!=b)){
    found = not_covered(pt,data)
    if(found){
      print(pt)
      return(pt)
    }
    pt = pt + dr
  }
  return(-1) # If all pt's are covered by sensor range, return -1.
}

# Initialize storage for point.
pt = c()

# Iterate over the rows in the data matrix.
for(i in 1:nrow(data)){
  
  # Check the perimeter for each sensor coordinates within data.
  pt = check_peri(data,i)
  
  # If pt contains coordinates (instead of -1), stop the loop.
  if(length(pt)==2)break
}

# Print the so-called 'tuning frequency' of the point in standard notation.
print(format(pt[1]*4000000+pt[2], scientific = FALSE))