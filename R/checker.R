##################################################
## Project: Binomial
## Script purpose:  Private checker functions for binomail package.
## Description: No roxygen comments as these functions are not called by the user.
##################################################


#Description: Tests if input prob is a valid probability value and returns either true or false.
check_prob <- function(prob){
  
  if (!is.numeric(prob)){
    stop("invalid prob value")
  }
  if (prob >= 0 && prob <= 1){
    return(TRUE)
  }else{
    stop("p has to be a number between 0 and 1")
  }
  
}

#Description: Tests if input trials is a valid value for number of trials(n is a positive integer)
# and returns true or false
check_trials <- function(trials){
  
  if (is.numeric(trials)){
    trials <- as.integer(trials)
  }else{
    stop("invalid trials value")
  }
  
  if (trials > 0){
      return(TRUE)
  }else{
     stop("invalid trials value")
  }

}

#Description: Tests if inputs: success, and trials are valid (success is positive integer which is
# less than or equal to trials). Returns true or false.
check_success <- function(success, trials) {
  
  if(is.numeric(success)){
    
    for (i in 1:length(success)){
      if (!success[i] <= trials){
        stop("success cannot be greater than trials")
      }
    }
    
    return(TRUE)
    
  }else{
    
    stop("invalid success value")
    
  }
  
}
