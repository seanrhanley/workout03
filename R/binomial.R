##################################################
## Project: Binomial
## Script purpose:  User called functions in the binomial package.
## Description: Functionality of package, including methods.
##################################################

library(ggplot2)
library(stringr)
library(testthat)
library(roxygen2)

######### Main Functions ##################################3


#' @title bin_choose
#' @description Calculates the number of combinations in which k successes occur in n trials
#' @param n trials as integer greater than zero
#' @param k successes as non-negative integer
#' @return successes as an integer value
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' #10
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







#' @title bin_probability
#' @description finds probable number of successes
#' @param success numeric number of successes
#' @param trials number of trials
#' @param prob probability as number between 0 and 1
#' @return probability as a numeric value
#' @export
#' @examples
#' # probability of getting 2 successes in 5 trials
#' (assuming prob of success = 0.5)
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' probabilities of getting 2 or less successes in 5 trials
#' (assuming prob of success = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' 55 heads in 100 tosses of a loaded coin with 45% chance of heads
#' bin_probability(success = 55, trials = 100, prob = 0.45)

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






#' @title bin_distribution
#' @description finds the probability distribution
#' @param trials as number of trials
#' @param prob as probability number between 0 and 1
#' @return data.frame as class 'bindis'
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)

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







#' @title bin_cumulative
#' @description finds the cumulative probability that sums to 1
#' @param trials as number of trials
#' @param prob as probability number between 0 and 1
#' @return data.frame as class 'bincum'
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
#' plotting binomial cumulative distribution
#' dis2 <- bin_cumulative(trials = 5, prob = 0.5)
#' plot(dis2)
#' #shows a graph of distribution

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



#' @title bin_variable
#' @description creates an object of class "binvar"
#' @param trials as number of trials
#' @param prob as probability number between 0 and 1
#' @return object of class 'binvar'
#' @export
#' @examples
#' bin1 <- bin_variable(trials = 10, p = 0.3)
#' binsum1 <- summary(bin1)
#' binsum1

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


#' @title bin_mean
#' @description calculate the mean of binomial distribution(np)
#' @param trials as number of trials
#' @param prob as probability number between 0 and 1
#' @return mean as numeric value
#' @export
#' @examples
#' bin_mean(10, 0.3)

bin_mean <- function(trials, prob){

  check_trials(trials)
  check_prob(prob)
  return(trials * prob)

}


#' @title bin_variance
#' @description calculate variance of binomial dist. (np(1-p))
#' @param trials as number of trials
#' @param prob as probability number between 0 and 1
#' @return variance as numeric value
#' @export
#' @examples
#' bin_variance(10, 0.3)

bin_variance <- function(trials, prob){

  check_trials(trials)
  check_prob(prob)

  n <- trials
  p <- prob

  mean <- aux_mean(trials, prob)
  var <- mean * (1 - p)

  return(var)

}



#' @title bin_mode
#' @description calculate the most likely number of successes in n independent trials. (the floor of np + p)
#' @param trials as number of trials
#' @param prob as probability number between 0 and 1
#' @return return mode as numeric value
#' @export
#' @examples
#' bin_mode(10, 0.3)

bin_mode <- function(trials, prob){

  check_trials(trials)
  check_prob(prob)

  n <- trials
  p <- prob
  mean <- n * p
  m = floor(mean + p)

  return(m)

}



#' @title bin_skewness
#' @description calculate asymmetry of the probability dist. of a random variable about its mean.
#' @param trials as number of trials
#' @param prob as probability number between 0 and 1
#' @return skewness as numeric value
#' @export
#' @examples
#' bin_skewness(10, 0.3)

bin_skewness <- function(trials, prob){

  check_trials(trials)
  check_prob(prob)

  mean <- aux_mean(trials, prob)
  var <- aux_variance(trials, prob)
  sd <- sqrt(var)
  skew <- (1 - 2 * prob) / sd
  return(skew)

}



#' @title bin_kurtosis
#' @description calculate measure of "tailedness" of probability dist. of a random variable
#' @param trials as number of trials
#' @param prob as probability number between 0 and 1
#' @return kurtosis as numeric value
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)

bin_kurtosis <- function(trials, prob){

  check_trials(trials)
  check_prob(prob)

  var <- aux_variance(trials, prob)
  num <- 1 - ((6 * prob) * (1 - prob))
  kurt <- num / var

  return(kurt)

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

  bin <- list(
    trials = x[[1]],
    prob = x[[2]],
    mean = aux_mean(x[[1]], x[[2]]),
    variance = aux_variance(x[[1]], x[[2]]),
    mode = aux_mode(x[[1]], x[[2]]),
    skewness = aux_skewness(x[[1]], x[[2]]),
    kurtosis = aux_kurtosis(x[[1]], x[[2]])
  )

  class(bin) <- "summary.binvar"
  return(bin)

}

#' @export
print.summary.binvar <- function(x,...){

  print(" \"Summary Binomial\" ", quote = FALSE)
  writeLines(" \nParameters")
  line1 <- paste("- number of trials:", x[[1]], sep = " ")
  line2 <- paste("- prob of success :", x[[2]], sep = " ")
  print(line1)
  print(line2)
  writeLines(" \nMeasures")
  line_mean <- paste("- mean:", x[[3]], sep = " ")
  line_var <- paste("- variance:", x[[4]], sep = " ")
  line_mode <- paste("- mode:", x[[5]], sep = " ")
  line_skew <- paste("- skewness:", x[[6]], sep = " ")
  line_kurt <- paste("- kurtosis:", x[[7]], sep = " ")
  print(line_mean)
  print(line_var)
  print(line_mode)
  print(line_skew)
  print(line_kurt)


}



################## Private Auxillary Functions ######################################################

#calculate the mean of binomial distribution(np)
aux_mean <- function(trials, prob){
  return(trials * prob)
}

#calculate variance of binomial dist. (np(1-p))
aux_variance <- function(trials, prob){

  n <- trials
  p <- prob

  mean <- aux_mean(trials, prob)
  var <- mean * (1 - p)

  return(var)

}

#calculate the most likely number of successes in n independent trials. (the floor of np + p)
aux_mode <- function(trials, prob){

  n <- trials
  p <- prob
  mean <- n * p
  m = floor(mean + p)

  return(m)
}

#calculate asymmetry of the probability dist. of a random variable about its mean.
aux_skewness <- function(trials, prob){

  mean <- aux_mean(trials, prob)
  var <- aux_variance(trials, prob)
  sd <- sqrt(var)
  skew <- (1 - 2 * prob) / sd
  return(skew)

}

#calculate measure of "tailedness" of probability dist. of a random variable
aux_kurtosis <- function(trials, prob){

  var <- aux_variance(trials, prob)
  num <- 1 - ((6 * prob) * (1 - prob))
  kurt <- num / var

  return(kurt)

}

