##################################################
## Project: Binomial
## Script purpose:  User called functions in the binomial package.
## Description: Functionality of package, including methods.
##################################################


#' @title Number of successes
#' @description Calculates the number of combinations in which k successes occur in n trials
#' @param n trials as integer greater than zero
#' @param k successes as non-negative integer
#' @return successes as an integer value
#' @export
#' @examples 
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
bin_choose <- function(n, k){
  
  if (k > n){
    stop("k cannot be greater than n")
  }
  if (n < 0 || k < 0){
    stop("neither k nor n can be negative values")
  }
  
  #n choose k, avoid loops, can't use choose()
  numerator <- factorial(n)
  denom <- factorial(k) * factorial((n - k))
  combos <- numerator / denom
  
  return(combos)
  
}



#' @title
#' @description 
#' @param 
#' @return 
#' @export
#' @examples
bin_probability <- function(success, trials, prob){
  
  # probability of getting 2 successes in 5 trials 
  # (assuming prob of success = 0.5) 
  #bin_probability(success = 2, trials = 5, prob = 0.5)
  ## [1] 0.3125
  # probabilities of getting 2 or less successes in 5 trials 
  # (assuming prob of success = 0.5) 
  #bin_probability(success = 0:2, trials = 5, prob = 0.5)
  ## [1] 0.03125 0.15625 0.31250
  # 55 heads in 100 tosses of a loaded coin with 45% chance of heads 
  # bin_probability(success = 55, trials = 100, prob = 0.45) 
  ## [1] 0.01075277
  
  
  #checks
  if (!check_trials(trials)){
    stop("invalid trials value")
  }
  if (!check_prob(prob)){
    stop("invalid probability value")
  }
  if (!check_success(success, trials)){
    stop("invalid success value")
  }
  
  p_fail <- (1 - prob) ^ (trials - success)
  p_success <- prob ^ success
  combo <- bin_choose(trials, success)
  probability <- combo * p_success * p_fail
  return(probability)
  
}

#' @title
#' @description 
#' @param 
#' @return 
#' @export
#' @examples
bin_distribution <- function(trials, prob){

  
  
}

#' @title
#' @description 
#' @param 
#' @return 
#' @export
#' @examples
bin_cumulative <- function(trials, prob){
  
  
  
}

#Auxillary check functions:
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