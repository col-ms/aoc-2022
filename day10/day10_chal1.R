data <- readLines("inputs/day10_input.txt")

x <- 1
cycle <- 0
poi <- seq(20, 220, 40)
signal_strengths <- vector()

for(l in data){
  if(l == "noop"){
    cycle = cycle + 1
    if(any(cycle == poi)){
      signal_strengths = c(signal_strengths, cycle * x)
    }
  } else {
    amt = strsplit(l, ' ')[[1]][2]
    for(n in 1:2){
      cycle = cycle + 1
      if(any(cycle == poi)){
        signal_strengths = c(signal_strengths, cycle * x)
      }
    }
    x = x + as.numeric(amt)
  }
}

print(sum(signal_strengths))