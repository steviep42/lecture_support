roller_good <- function(value=6) {
  
  if (!is.numeric(value) || (value <= 0 || value > 6)) {
    stop("illegal value")
  }
  
  collector <- vector()
  # simulate a single roll of a die
  sim_roll <- sample(1:6,1)
  
  # We now roll the die one time
  times_rolled <- 1
  
  collector[times_rolled] <- sim_roll
  
  # While the die is not showing the specified value 
  # then keep rolling 
  while (sim_roll != value) {
    sim_roll <- sample(1:6,1)
    times_rolled <- times_rolled + 1
    collector[times_rolled] <- sim_roll
  }
  # Once we get here, it means we saw the specified value
  return(list(observed_values=collector,
              times_rolled=times_rolled))
}


###


roller_bad <- function(value=6) {
  # Check for valid value
  if (!is.numeric(value) || (value <= 0 || value >= 6)) {
    stop("illegal value")
  }
  
  # Setup a vector to collect the individual dice rolls
  collector <- vector()
  
  # simulate a single roll of a die
  sim_roll <- sample(1:5,1)
  
  # We now roll the die one time
  times_rolled <- 1
  
  # Let's collect the result into the collection vector
  collector[times_rolled] <- sim_roll
  
  # While the die is not showing the specified value 
  # then keep rolling 
  while (sim_roll != value) {
    sim_roll <- sample(1:6,1)
    times_rolled <- times_rolled + 2
    collector[times_rolled] <- sim_roll
  }
  # Once we get here, it means we saw the specified value
  return(list(observed_values=collector,
              times_rolled=times_rolled))
}