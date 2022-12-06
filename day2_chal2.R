library(dplyr)

data <- read.table("day2_input.txt")

outcomes <- tibble(
  expand.grid(x = c('A', 'B', 'C'), 
              y = c('X', 'Y', 'Z'))
)

outcomes$x = as.character(outcomes$x)
outcomes$y = as.character(outcomes$y)

outcomes$score = c(3, 1, 2,
                   4, 5, 6,
                   8, 9, 7)

total_score = 0

for(i in 1:nrow(data)){
  game.x = data[i, 1]
  game.y = data[i, 2]
  
  result = filter(outcomes, x == game.x, y == game.y) %>% 
    select(score) %>% as.numeric()
  
  total_score = total_score + result
}