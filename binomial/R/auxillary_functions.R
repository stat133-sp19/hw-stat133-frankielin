# Title: Auxillary Functions
# Inputs: N/A
# Outputs: aux_mean(), aux_variance(), aux_mode(), aux_skew(), aux_kurtosis()
# Description: Produces the auxillary funcitons used for calculations in the main functions.
# These functions are used in the main functions to perform higher level calculations such as means, variances, modes, etc.

#private function: aux_mean()
aux_mean <- function(trials, prob){
  return(trials * prob)
}

#private function: aux_variance()
aux_variance <- function(trials, prob){
  return(prob*(1-prob)*trials)
}

#private function: aux_mode()
aux_mode <- function(trials, prob){
  if(prob == 0){
    return(0)
  }
  if(prob == 1){
    return(trials)
  }
  pre_m = trials*prob + prob
  if (pre_m %% 1 == 0) {
    return(c(floor(pre_m), floor(pre_m)-1))
  }
  return(floor(pre_m))
}

#private function: aux_skew()
aux_skewness <- function(trials, prob){
  s = (1-2*prob)/sqrt(aux_variance(trials, prob))
  return(round(s,7))
}

#private function: aux_kurtosis
aux_kurtosis <- function(trials, prob){
  k = (1 - 6*prob*(1 - prob))/(aux_variance(trials,prob))
  return(round(k,7))
}
