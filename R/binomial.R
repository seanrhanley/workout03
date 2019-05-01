##################################################
## Project: Binomial
## Script purpose:  User called functions in the binomial package.
## Description: Functionality of package, including methods.
##################################################

library(ggplot2)
library(stringr)


######### Main Functions ##################################3


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
  
  if (length(k[k <= n]) != length(k)){
    stop("k cannot be greater than n")
  }
  
  if (n < 0 || length(k[k < 0]) != 0){
    stop("neither k nor n can be negative values")
  }
  
  numerator <- factorial(n)
  denom <- rep(0, length(k))
  denom <- factorial(k) * factorial((n - k))
  combos <- rep(0, length(k))
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
  probability <- rep(0, length(combo))
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

p <- rep(0, trials + 1)
p <- bin_probability(0:trials, trials, prob)

success <- 0:trials

df <- data.frame(
      success = success,
      probability = p
)

class(df) = c("bindis", "data.frame")

return(df)

}







#' @title
#' @description 
#' @param 
#' @return 
#' @export
#' @examples
bin_cumulative <- function(trials, prob){
  
  dist <- bin_distribution(trials, prob)
  cumlv <- rep(0, length(dist$probability))
  
  df <- data.frame(
    success = dist$success,
    probability = dist$probability,
    cumlv = cumsum(dist$probability)
    
  )
  class(df) <- c("bincum", "data.frame")
  return(df)
  
}



#' @title
#' @description 
#' @param 
#' @return 
#' @export
#' @examples
bin_variable <- function(trials, prob){

  check_trials(trials)
  check_prob(prob)
  
  binvar <- list(
    trials <- trials,
    prob <- prob
  )
  
  class(binvar) <- "binvar"
  return(binvar)

}










############## Auxillary check functions #########################

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







###########  Methods  #######################

#' @export

plot.bindis <- function(x,...){
  
  ggplot2::ggplot(x) +
    ggplot2::aes(x = success, y = probability) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") + 
    ggplot2::xlab("Successes") + 
    ggplot2::ylab("Probability")    
  
}

#' @export
plot.bincum <- function(x,...){
  
  ggplot2::ggplot(x) +
    ggplot2::aes(x = success, y = cumlv) +
    ggplot2::geom_path(linejoin = "mitre", lineend = "round", colour = 'steelblue', size = 3) +
  ggplot2::xlab("Successes") +
  ggplot2::ylab("Probability") +
    ggplot2::theme_minimal()
  
}

#' @export
print.binvar <- function(x,...){
  
  print(" \"Binomial variable\" ", quote = FALSE)
  writeLines(" \nParameters")
  line1 <- paste("- number of trials:", x[[1]], sep = " ")
  line2 <- paste("- prob of success :", x[[2]], sep = " ")
  print(line1)
  print(line2)
  
}

#' @export
summary.binvar <- function(x,...){
  
  
  
}

#' @export
print.summary.binvar <- function(x,...){
  
  
  
}

