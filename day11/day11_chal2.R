# Read in input data from file, remove empty elements
data <- readLines("inputs/day11_input.txt") |> trimws()
data <- data[data != '']

# Initialize empty list to store information about each monkey
monkeys <- list()

# Define a function to extract numeric values from a string
get_nums <- function(string){
  return(as.numeric(stringr::str_extract_all(string, "\\d+")[[1]]))
}

# Define a function to apply a rule to an "old" value to produce a "new" value
get_new <- function(old, rule){
  lhs = old
  rhs = ifelse(rule[3] == 'old', old, as.numeric(rule[3]))
  result = ifelse(rule[2] == '+', sum(lhs,rhs), prod(lhs,rhs))
  return(result)
}

# Initialize empty vector to store all of the "test" values from the input data
div_list <- as.numeric()

# Extract information about each monkey from the input data
for(i in seq(1, length(data), 6)){
  id = gsub("\\D", "", data[i])
  items = get_nums(data[i+1])
  rule = strsplit(data[i+2], "= ")[[1]][2] |> strsplit(split = " ") |> unlist()
  test = get_nums(data[i+3])
  t_targ = as.character(get_nums(data[i+4]))
  f_targ = as.character(get_nums(data[i+5]))
  
  # Store information about each monkey in the "monkeys" list
  monkeys[[id]] = list("items" = items, "rule" = rule, "test" = test, 
                       "t_targ" = t_targ, "f_targ" = f_targ,
                       "n_insp" = 0)
  
  # Add the current "test" value to the "div_list" vector
  div_list = c(div_list, test)
}

# function to inspect items for a given monkey and apply the rule to each item
inspect <- function(monkeys, id, div_list){
  
  # Extract the list of items for the current monkey
  item_list = monkeys[[id]]$items
  
  # If the list of items is empty, return the list of monkeys unchanged
  if(length(item_list) == 0){return(monkeys)}
  
  # Increment the number of inspections for current monkey by item list length
  monkeys[[id]]$n_insp = monkeys[[id]]$n_insp + length(item_list)
  
  # Extract the rule, test, t_targ, and f_targ for the current monkey
  rule = monkeys[[id]]$rule
  test = monkeys[[id]]$test
  t_targ = monkeys[[id]]$t_targ
  f_targ = monkeys[[id]]$f_targ
  
  # Iterate over each item in the list of items for the current monkey
  for(n in 1:length(item_list)){
    
    # Apply the rule to the current item to produce a new item
    new = get_new(item_list[n], rule)
    
    # Modulo the new item by the product of all "test" values in the input data
    new = new%%prod(div_list)
    
    # If new item meets the test, pass to the monkey specified by t_targ
    if(new %% test == 0){
      monkeys[[t_targ]]$items = c(monkeys[[t_targ]]$items, new)
    } 
    
    # If new item does not meet the test, pass to the monkey specified by f_targ
    else {
      monkeys[[f_targ]]$items = c(monkeys[[f_targ]]$items, new)
    }
  }
  
  # Set the list of items for the current monkey to be empty
  monkeys[[id]]$items = as.numeric()
  
  # Return the modified list of monkeys
  return(monkeys)
}

# Iterate for 10000 rounds
for(rounds in 1:10000){
  
  # Iterate over each monkey
  for(m in 0:(length(monkeys)-1)){
    
    # Inspect the items for the current monkey
    monkeys = inspect(monkeys, as.character(m), div_list)
  }
}

# Initialize empty vector to store the number of inspections for each monkey
top2 <- as.numeric()

# Iterate over each monkey, store the number of inspections in the "top2" vector
for(m in 1:length(monkeys)){
  top2 = c(top2, unlist(monkeys[[m]][6]))
}

# Calculate the product of the two monkeys who inspected the most items
# and print the result to the console
print(prod(sort(top2, decreasing = TRUE)[1:2]))