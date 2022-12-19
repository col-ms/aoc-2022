# data_import reads the input data into a tabular format
# and sets the column classes and names
data_import <- function(){
  return(read.table("inputs/day9_input.txt", 
                    colClasses = c("character", "numeric"),
                    col.names = c("Dir", "Num")))
}

# get_hdir returns a vector representing a movement in the specified direction
get_hdir <- function(hdir){
  return(dplyr::case_when(hdir == "L" ~ c(-1,0),
                          hdir == "R" ~ c(1, 0),
                          hdir == "U" ~ c(0, 1),
                          hdir == "D" ~ c(0,-1)))
}

# t_move takes in the current position of the head (hc) and the tail (tc)
# and returns the position of the tail as it follows the head
t_move <- function(hc, tc){
  
  # calculate the Euclidean distance between head and tail
  ht_dist <- dist(rbind(hc,tc)) |> as.numeric()
  
  # if the distance between head and tail is greater than the maximum distance
  # that can be covered in one step (sqrt(2)), move the tail towards the head
  
  # can also be thought of as: is the tail within the perimeter of the head
  # location? if not, move it so that it is
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
      
    } else if(hc[1] > tc[1] && hc[2] > tc[2]){ # is head (right + up) of tail
      tc = tc + 1
    } else if(hc[1] > tc[1] && hc[2] < tc[2]){ # is head (right + down) of tail
      tc = tc + c(1, -1)
    } else if(hc[1] < tc[1] && hc[2] < tc[2]){ # is head (down + left) of tail
      tc = tc - 1
    } else {                                   # is head (up + left) of tail
      tc = tc + c(-1, 1)
    }
  }
  return(tc)
}