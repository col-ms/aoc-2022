data <- readLines("inputs/day10_input.txt")
#data <- readLines("day10/test_data.txt")

updateSprite <- function(x){
  if(x < 0){return(rep('.', 40))}
  x = x + 1
  line = rep('.', 40)
  line[c(x-1,x,x+1)] = "#"
  return(line[1:40])
}

writeScreen <- function(cycle, spritePos){
  return(ifelse(any(cycle%%40 == which(spritePos == '#')), '#', '.'))
}

csConv <- function(cycle){
  return(ifelse(cycle%%40 == 0, 40, cycle%%40))
}

prtSc <- function(screen){
  for(i in 1:6){
    print(paste(screen[i,], collapse = ''))
  }
}

x <- 1
cycle <- 0
screen <- matrix(NA, nrow = 6, ncol = 40)
spritePos <- updateSprite(x)

for(l in data){
  
  if(l == "noop"){
    cycle = cycle + 1
    screen[ceiling(cycle/40),csConv(cycle)] = writeScreen(cycle, spritePos)
    
  } else {
    amt = as.numeric(strsplit(l, ' ')[[1]][2])
    
    cycle = cycle + 1
    screen[ceiling(cycle/40),csConv(cycle)] = writeScreen(cycle, spritePos)
    
    cycle = cycle + 1
    screen[ceiling(cycle/40),csConv(cycle)] = writeScreen(cycle, spritePos)
    
    x = x + amt
    spritePos = updateSprite(x)
  }
}

prtSc(screen)
