# read in the data from the input file
data <- readLines("inputs/day10_input.txt")

# function to write a character to screen based on cycle and value of x
writeScreen <- function(cycle, x){
  return(ifelse(any(cycle%%40 == c(x-1,x,x+1)), '#', '.'))
}

# function to print the screen
prtSc <- function(pxls){
  
  # convert the character vector to a matrix with 40 columns
  screen = matrix(pxls, ncol = 40, byrow = TRUE)
  
  # loop through the rows of the matrix and print each row as a string
  for(i in 1:6){
    print(paste(screen[i,], collapse = ''))
  }
}

# initialize variables
x <- 1
cycle <- 0
screen <- as.character()

# loop through each line in the data
for(l in data){
  
  # if the line is "noop", increment cycle and write to the screen
  if(l == "noop"){
    
    screen = c(screen, writeScreen(cycle, x))
    cycle = cycle + 1
    
  } else {
    
    # if the line is not "noop", split it on the space to get the amount
    amt = as.numeric(strsplit(l, ' ')[[1]][2])
    
    # increment cycle and write to screen, twice
    for(n in 1:2){
      screen = c(screen, writeScreen(cycle, x))
      cycle = cycle + 1
    }
  
    # increment x by the amount from instruction
    x = x + amt
  }
}

# print the resulting screen
prtSc(screen)