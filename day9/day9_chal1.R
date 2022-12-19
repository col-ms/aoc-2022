data <- read.table("inputs/day9_input.txt", 
                   colClasses = c("character", "numeric"),
                   col.names = c("Dir", "Num"))

# data <- data.frame(Dir = c("R", "U", "L", "D", "R", "D", "L", "R"),
#                    Num = c(4, 4, 3, 1, 4, 1, 5, 2))

hpos <- c(0,0)
tpos <- c(0,0)
tpos_list <- tpos

get_hdir <- function(hdir){
  return(dplyr::case_when(hdir == "L" ~ c(-1,0),
                          hdir == "R" ~ c(1, 0),
                          hdir == "U" ~ c(0, 1),
                          hdir == "D" ~ c(0,-1)))
}

t_move <- function(hc, tc){
  
  ht_dist <- dist(rbind(hc,tc)) |> as.numeric()
  
  if(ht_dist > sqrt(2)){
    if(tc[1] == hc[1]){ # if positions have same x value
      if(hc[2] > tc[2]){
        tc[2] = tc[2] + 1
      } else {
        tc[2] = tc[2] - 1
      }
    } else if(tc[2] == hc[2]){ # if positions have same y value
      if(hc[1] > tc[1]){
        tc[1] = tc[1] + 1
      } else {
        tc[1] = tc[1] - 1
      }
    } else if(hc[1] > tc[1] && hc[2] > tc[2]){
      tc = tc + 1
    } else if(hc[1] > tc[1] && hc[2] < tc[2]){
      tc = tc + c(1, -1)
    } else if(hc[1] < tc[1] && hc[2] < tc[2]){
      tc = tc - 1
    } else {
      tc = tc + c(-1, 1)
    }
  }
  
  return(tc)

}

for(i in 1:nrow(data)){
  
  move_dir <- get_hdir(data[i,1])

  for(n in 1:data[i,2]){
    hpos = hpos + move_dir
    tpos = t_move(hpos, tpos)
    tpos_list = rbind(tpos_list, tpos)
  }

}

print(paste("Unique positions visited:", nrow(unique(tpos_list))))
