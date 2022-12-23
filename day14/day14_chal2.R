data <- readLines("inputs/day14_input.txt")
data <- lapply(strsplit(data, " -> "),
               function(x) apply(do.call(rbind, strsplit(x,',')),2,as.integer))

cave <- matrix(0, ncol = 600, nrow = 200)

for(i in seq_along(data)){
  for(j in 2:nrow(data[[i]])){
    cave[data[[i]][j-1,2]:data[[i]][j,2],
         data[[i]][j-1,1]:data[[i]][j,1]] = 1
  }
}

cave[max(which(apply(cave, 1, sum)!=0))+2,] = 1

sand <- 0

x <- 0
y <- 500

lx <- x
ly <- y

while(TRUE){
  
  x = lx[length(lx)]
  y = ly[length(ly)]
  
  if(y+3 == ncol(cave)){
    cave = cbind(cave, cave[,1])
  }
  
  repeat{

    if(x>=nrow(cave)){stop(sand)}
    if(cave[x+1,y]==0){
      lx = c(lx,x)
      ly = c(ly,y)
      x = x + 1
    } else if(cave[x+1,y-1]==0){
      lx = c(lx,x)
      ly = c(ly,y)
      x = x + 1
      y = y - 1
    } else if(cave[x+1,y+1]==0){
      lx = c(lx,x)
      ly = c(ly,y)
      x = x + 1
      y = y + 1
    } else {
      if(x == 0){stop(sand+1)}
      if(cave[x,y]!=2){
        cave[x,y] = 2
        cat(sand, "\r")
        sand = sand + 1
      }
      lx = lx[-length(lx)]
      ly = ly[-length(ly)]
      break
    }
  }
}