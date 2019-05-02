# Title: Check Functions
# Inputs: N/A
# Outputs: check_prob(), check_trials(), check_success()
# Description: These functions check the inputs onto our main functions in order to make sure that the inputs are valid. These are all private functions.

# private function: check_prob()
check_prob <- function(prob){
  if(!is.numeric(prob)){
    stop("invalid prob value - must be numeric")
  } else if (prob> 1 | prob < 0){
    stop("prob must be between 0 and 1")
  }
  return(TRUE)
}

# private function: check_trials()
check_trials <- function(trials){
  if(!is.numeric(trials)){
    stop("invalid trials value - must be numeric")
  } else if(trials != floor(trials)){
    stop("invalid trials value - must be whole number")
  } else if(trials < 0){
    stop("invalid trials value- cannot be negative")
  }
  return(TRUE)
}

#private function: check_success()
check_success <- function(success, trials){
  if(!is.numeric(success)){
    stop("invalid success value - must be numeric")
  }
  else if(sum(success == floor(success)) != length(success)){
    stop("invalid success value - must be whole number")
  }
  check_trials(trials)
  out <- (success >= 0 & success <= trials)
  if(sum(out) != length(success)){
    stop("'success cannot be greater than trials")
  }
  return(TRUE)
}
