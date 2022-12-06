inv_tots <- as.numeric(readLines('inputs/day1_inv_totals.txt'))

top3 <- sum(sort(inv_tots, decreasing = TRUE)[1:3])