# loads the is_ordered function from external utilities script
source("day13/day13_utils.R")

data <- readLines("inputs/day13_input.txt") # read in data from input file
data <- data[data!=""]                      # remove empty lines

# converts each element of data to a list using the fromJSON function
parsed <- sapply(data, 
                 function(x) jsonlite::fromJSON(x, simplifyVector = FALSE))

# call is_ordered on every element of parsed data, with [[2]] | [[6]] as 
# the argument for r. Sums the resulting vector and adds 1 or 2 respectively.
# This is done to find where [[2]] or [[6]] would reside (the index) if inserted
# into a sorted list of all the parsed data
first_pos <- sum(sapply(parsed, is_ordered, r = list(list(2))))+1
second_pos <- sum(sapply(parsed, is_ordered, r = list(list(6))))+2

# print the product of the index of [[2]] and [[6]] found above
print(prod(first_pos, second_pos))