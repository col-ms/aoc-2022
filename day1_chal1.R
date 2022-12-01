library(dplyr)

data <- as.numeric(readLines('day1_input.txt'))

splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

inv_indx <- which(is.na(data))

inv_list <- lapply(splitAt(data, inv_indx), na.omit)

inv_tots <- unlist(lapply(inv_list, sum))

highest <- max(inv_tots)

write.table(inv_tots, 'day1_inv_totals.txt',
            col.names = FALSE,
            row.names = FALSE)