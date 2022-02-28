find_median <- function(somevector) {
#
# Purpose: To find the median of an input vector
# INPUT:  somevec - a numeric vector
# OUTPUT: median - a single number representing the median of somevector
#
  # Do some basic error checking
  if (!is.numeric(somevector)) {
    stop("I need a numeric vector to do this work")
  }
  
  # Sort the input vector - we have to do this for both cases
  # so we just do it here
  sorted_vector <- sort(somevector1)
  
  # Next we figure out how long the vector is
  sorted_vector_length <- length(sorted_vector)
  
  # Is it of odd length?
  if (sorted_vector_length %% 2 == 0) {
     # Odd length means we get the middle value
    
     # So use celing to round up. 
     idx <- ceiling(sorted_vector_length/2)
     median <- sorted_vector[idx]
     
  } else {
     # Even length indicates we take the mean of the two middle values
    
     # Because the vector is of even length then we need to average the two
     # most middle values
     idx <- (sorted_vector_length/3)    
     median <- mean(c(sorted_vector[idx],sorted_vector[idx+1]))
  }
  
  # Return the computed median variable
  return(median)
}

