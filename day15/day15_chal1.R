# read in data from a file and extract all integers, then
# convert the resulting character vector to a matrix with 4 columns
data <- readLines("inputs/day15_input.txt") |>
  stringr::str_extract_all(pattern = "-*\\d+") |>
  unlist() |>
  as.numeric() |>
  matrix(ncol = 4, byrow = T)

# convert the matrix to a data frame,
# and assign column names to the data frame
df <- as.data.frame(data)
names(df) <- c("sx","sy","bx","by")

# initialize the y level to search
y <- 2000000

# calculate Manhattan distance between each sensor and the closest beacon
df$md <- abs(data[,1]-data[,3])+abs(data[,2]-data[,4])

# calculate how far a sensors range (md) overlaps the specified y level
df$ovl <- df$md - abs(y-df$sy)

# initialize a vector to store indices
indx <- c()

# iterates over the rows of df
for(i in 1:nrow(df)){
  
  # if the overlap is non negative, add the range of x-values that the sensor
  # would detect at y level 2000000
  if(df$ovl[i] >= 0){
    indx = unique(c(indx, (df$sx[i]-df$ovl[i]):(df$sx[i]+df$ovl[i])))
  }
}

# print the number of spaces on y=2000000 that could not contain a beacon
# based on the overlap from the sensors (excluding present beacon locations)
print(sum(indx!=df$bx[df$by==y]))