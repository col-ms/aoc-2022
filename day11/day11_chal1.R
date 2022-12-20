data <- readLines("inputs/day11_input.txt") |> trimws()
#data <- readLines("day11/d11_testdata.txt") |> trimws()
data <- data[data != '']

monkeys <- list()

get_nums <- function(string){
  return(as.numeric(stringr::str_extract_all(string, "\\d+")[[1]]))
}

get_new <- function(old, rule){
  lhs = old
  rhs = ifelse(rule[3] == 'old', old, as.numeric(rule[3]))
  result = ifelse(rule[2] == '+', sum(lhs,rhs), prod(lhs,rhs))
  return(result)
}

for(i in seq(1, length(data), 6)){
  id = gsub("\\D", "", data[i])
  items = get_nums(data[i+1])
  rule = strsplit(data[i+2], "= ")[[1]][2] |> strsplit(split = " ") |> unlist()
  test = get_nums(data[i+3])
  t_targ = as.character(get_nums(data[i+4]))
  f_targ = as.character(get_nums(data[i+5]))
  
  monkeys[[id]] = list("items" = items, "rule" = rule, "test" = test, 
                       "t_targ" = t_targ, "f_targ" = f_targ,
                       "n_insp" = 0)
}

inspect <- function(monkeys, id){
  item_list = monkeys[[id]]$items
  if(length(item_list) == 0){return(monkeys)}
  monkeys[[id]]$n_insp = monkeys[[id]]$n_insp + length(item_list)
  rule = monkeys[[id]]$rule
  test = monkeys[[id]]$test
  t_targ = monkeys[[id]]$t_targ
  f_targ = monkeys[[id]]$f_targ
  for(n in 1:length(item_list)){
    new = get_new(item_list[n], rule)
    new = floor(new / 3)
    if(new %% test == 0){
      monkeys[[t_targ]]$items = c(monkeys[[t_targ]]$items, new)
    } else {
      monkeys[[f_targ]]$items = c(monkeys[[f_targ]]$items, new)
    }
  }
  monkeys[[id]]$items = as.numeric()
  return(monkeys)
}

for(rounds in 1:20){
  for(m in 0:(length(monkeys)-1)){
    monkeys = inspect(monkeys, as.character(m))
  }
}

top2 <- as.numeric()

for(m in 1:length(monkeys)){
  top2 = c(top2, unlist(monkeys[[m]][6]))
}

prod(sort(top2, decreasing = TRUE)[1:2])