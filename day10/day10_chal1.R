# read in he data from the input file
data <- readLines("inputs/day10_input.txt")

# initialize variables
x <- 1                        # register in simulated pc
cycle <- 0                    # cpu cycle counter
poi <- seq(20, 220, 40)       # points of interests (for cycle counter)
signal_strengths <- vector()  # to hold cpu signal strengths at POIs

# loop through each line in the data
for(l in data){
  
  # if the line is "noop" (no operation), increment cycle & check if POI
  if(l == "noop"){
    cycle = cycle + 1
    if(any(cycle == poi)){
      signal_strengths = c(signal_strengths, cycle * x)
    }
  } 
  
  # if the line is not "noop", split it to 
  # get the amount to be added to register x
  else {
    amt = strsplit(l, ' ')[[1]][2]
    
    # increment cycle twice and check for POIs
    for(n in 1:2){
      cycle = cycle + 1
      if(any(cycle == poi)){
        signal_strengths = c(signal_strengths, cycle * x)
      }
    }
    
    # increment x by amount from instruction
    x = x + as.numeric(amt)
  }
}

# print the sum of the signal strengths
print(sum(signal_strengths))