# read in input data from file, remove empty elements
data <- readLines("inputs/day11_input.txt") |> trimws()
data <- data[data != '']

# initialize empty list to store information about each monkey
monkeys <- list()

# define a function to extract numeric values from a string
get_nums <- function(string){
  return(as.numeric(stringr::str_extract_all(string, "\\d+")[[1]]))
}

# define a function to apply a rule to an old value to produce a new value
get_new <- function(old, rule){
  lhs = old
  rhs = ifelse(rule[3] == 'old', old, as.numeric(rule[3]))
  result = ifelse(rule[2] == '+', sum(lhs,rhs), prod(lhs,rhs))
  return(result)
}

# extract information about each monkey from the input data
for(i in seq(1, length(data), 6)){
  id = gsub("\\D", "", data[i])
  items = get_nums(data[i+1])
  rule = strsplit(data[i+2], "= ")[[1]][2] |> strsplit(split = " ") |> unlist()
  test = get_nums(data[i+3])
  t_targ = as.character(get_nums(data[i+4]))
  f_targ = as.character(get_nums(data[i+5]))
  
  # store information about each monkey in the 'monkeys' list
  monkeys[[id]] = list("items" = items, "rule" = rule, "test" = test, 
                       "t_targ" = t_targ, "f_targ" = f_targ,
                       "n_insp" = 0)
}

# define a function to inspect the items for a particular monkey and apply
# a given rule to each item
inspect <- function(monkeys, id){
  
  # extract the list of items for the current monkey
  item_list = monkeys[[id]]$items
  
  # if the list of items is empty, return the list of monkeys unchanged
  if(length(item_list) == 0){return(monkeys)}
  
  # increment the number of inspections for current monkey by item list length
  monkeys[[id]]$n_insp = monkeys[[id]]$n_insp + length(item_list)
  
  # extract the rule, test, t_targ, and f_targ for the current monkey
  rule = monkeys[[id]]$rule
  test = monkeys[[id]]$test
  t_targ = monkeys[[id]]$t_targ
  f_targ = monkeys[[id]]$f_targ
  
  # iterate over each item in the list of items for the current monkey
  for(n in 1:length(item_list)){
    
    # apply the rule to the current item to produce a new item
    new = get_new(item_list[n], rule)
    
    # divide the new item by 3 and take the floor of the result (round down)
    new = floor(new / 3)
    
    # if the new item meets the test, pass it to monkey specified by t_targ
    if(new %% test == 0){
      monkeys[[t_targ]]$items = c(monkeys[[t_targ]]$items, new)
    } 
    
    # if new item does not meet test, pass to monkey specified by f_targ
    else {
      monkeys[[f_targ]]$items = c(monkeys[[f_targ]]$items, new)
    }
  }
  
  # set the list of items for the current monkey to be empty
  monkeys[[id]]$items = as.numeric()
  
  # return the modified list of monkeys
  return(monkeys)
}

# iterate over reach round of the game
for(rounds in 1:20){
  
  # iterate over each monkey
  for(m in 0:(length(monkeys)-1)){
    
    # inspect the items for the current monkey
    monkeys = inspect(monkeys, as.character(m))
  }
}

# initialize empty vector to store the number of inspections for each monkey
top2 <- as.numeric()

# iterate over each monkey and store number of inspections in 'top2' vector
for(m in 1:length(monkeys)){
  top2 = c(top2, unlist(monkeys[[m]][6]))
}

# print the product of the two highest values in the 'top2' vector
print(prod(sort(top2, decreasing = TRUE)[1:2]))