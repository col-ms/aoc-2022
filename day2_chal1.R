data <- read.table('day2_input.txt')

# A = Rock = X
# B = Paper = Y
# C = Scissors = Z

rps <- function(opp, me){
  
  score = 0
  
  # determine points for shape played
  if(me == "X"){
    score = score + 1
  } else if(me == "Y"){
    score = score + 2
  } else if(me == "Z"){
    score = score + 3
  }
  
  # determine outcome of match
  score = score + ifelse(
    (me == "X" && opp == "C") ||
    (me == "Y" && opp == "A") ||
    (me == "Z" && opp == "B"),
    6, 0)
  
  score = score + ifelse(
    (me == "X" && opp == "A") ||
    (me == "Y" && opp == "B") ||
    (me == "Z" && opp == "C"), 
    3, 0)
  
  return(score)
}

scores = vector()

for(i in 1:nrow(data)){
  scores[i] = rps(data$V1[i], data$V2[i])
}

sum(scores)
