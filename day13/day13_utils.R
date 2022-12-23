# is_ordered is a function that takes in two lists, l and r, and returns
# a logical value indicating whether l is ordered before r

is_ordered <- function(l, r){
  
  # if both l and r are empty, return NA
  if(length(l)==0 && length(r)==0){
    return(NA)
  } else if(length(l)==0){ # if l is empty, return TRUE
    return(TRUE)
  } else if(length(r)==0){ # if r is empty, return FALSE
    return(FALSE)
  }
  
  # if both l and r have numeric elements, compare the elements
  if(is.numeric(l[[1]]) && is.numeric(r[[1]])){
    
    # if all elements of l are less than elements of r, return TRUE
    if(all(l[[1]] < r[[1]])){
      return(TRUE)
    } 
    
    # if any elements of l are greater than r, return FALSE
    else if(any(l[[1]] > r[[1]])){
      return(FALSE)
    } 
    
    # if the elements are equal, recursively call is_ordered with the rest
    # of the lists
    else {
      l <- l[-1]
      r <- r[-1]
      return(is_ordered(l,r))
    }
  }
  
  # if both l and r have list elements, compare the lists
  if(is.list(l[[1]]) && is.list(r[[1]])){
    
    # recursively call is_ordered on the list elements
    ord <- is_ordered(l[[1]],r[[1]])
    
    # if the result is NA, recursively call is_ordered on the rest of the lists
    if(is.na(ord)){
      l <- l[-1]
      r <- r[-1]
      return(is_ordered(l,r))
    } 
    
    # otherwise, return the result of is_ordered on the list elements
    else {
      return(ord)
    }
  }
  
  # if l has a numeric element and r has a list element,
  # treat l as a list and then compare
  if(is.numeric(l[[1]]) && is.list(r[[1]])){
    l[[1]] <- list(l[[1]])
    return(is_ordered(l,r))
  }
  
  # if l has a numeric element and r has a list element,
  # treat l as a list and then compare
  if(is.list(l[[1]]) && is.numeric(r[[1]])){
    r[[1]] <- list(r[[1]])
    return(is_ordered(l,r))
  }
}