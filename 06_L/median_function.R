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
  sorted_vector <- sort(somevector)
  
  # Next we figure out if the vector is of even or odd length
  sorted_vector_length <- length(sorted_vector)
  
  if (sorted_vector_length %% 2 !=0) {
     # Odd length means we get the middle value
    
     idx <- ceiling(sorted_vector_length/2)
     median <- sorted_vector[idx]
     
  } else {
     # Even length which indicates we take the mean of the two middle values
    
     idx <- (sorted_vector_length/2)    
     median <- mean(c(sorted_vector[idx],sorted_vector[idx+1]))
  }
  
  # Return the computed median variable
  return(median)
}

##
# Given an array of strings strs, group the anagrams together. You can return the answer 
# in any order.

# An Anagram is a word or phrase formed by rearranging the letters of a different word or 
# phrase, typically using all the original letters exactly once.

strs = c("eat","tea","tan","ate","nat","bat")
strs = c("stop","pots","tops","full","fall")
str_list <- sapply(strs,function(x) strsplit(x,""))
exp_list <- lapply(str_list,sort)
col_list <- lapply(exp_list,paste,collapse="")
newvec <- sort(unlist(col_list))
names(newvec)

for (ii in 1:length(newvec)) {
  
}

for (ii in 1:length(strs)) {
   strsplit(strs[ii],"")
}