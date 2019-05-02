library(testthat)
source(../R/binomial.R)

context("check_prob()")

test_that("probability is a number between 0 and 1", {
  
  x <- 0.5;
  y <- -1;
  z <- 2;
  l <- 'a';
  
  expect_equal(check_prob(x), TRUE)
  expect_length(check_prob(x), 1)
  expect_error(check_prob(y), "p has to be a number between 0 and 1")
  expect_error(check_prob(l), "invalid prob value")
  
})


context("check_tirals()")
context("check_success()")

context("aux_mean()")
context("aux_variance()")
context("aux_mode()")
context("aux_skewness()")
context("aux_kurtosis()")

context("bin_choose()")
context("bin_probability()")
context("bin_distribution()")
context("bin_cumulative()")