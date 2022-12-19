data <- readLines("inputs/day10_input.txt")

writeScreen <- function(cycle, x){
  return(ifelse(any(cycle%%40 == c(x-1,x,x+1)), '#', '.'))
}

prtSc <- function(pxls){
  screen = matrix(pxls, ncol = 40, byrow = TRUE)
  for(i in 1:6){
    print(paste(screen[i,], collapse = ''))
  }
}

x <- 1
cycle <- 0
screen <- as.character()

for(l in data){
  
  if(l == "noop"){
    
    screen = c(screen, writeScreen(cycle, x))
    cycle = cycle + 1
    
  } else {
    amt = as.numeric(strsplit(l, ' ')[[1]][2])
    
    screen = c(screen, writeScreen(cycle, x))
    cycle = cycle + 1
    
    screen = c(screen, writeScreen(cycle, x))
    cycle = cycle + 1
    
    x = x + amt
  }
}

prtSc(screen)