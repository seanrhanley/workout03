##################################################
## Project: Binomial
## Script purpose:  Private auxillary functions for binomial package.
## Description: No roxygen comments as these functions are not called by the user. All functions take inputs: trials 
## and prob. They returned a corresponding value from the computed summary measure.
##################################################

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