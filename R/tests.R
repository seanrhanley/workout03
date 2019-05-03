library(testthat)
source("binomial.R")

context("Checkers")


test_that("check_prob is a number between 0 and 1", {
  
  x <- 0.5;
  y <- -1;
  z <- 2;
  l <- 'a';
  
  expect_equal(check_prob(x), TRUE)
  expect_length(check_prob(x), 1)
  expect_error(check_prob(y), "p has to be a number between 0 and 1")
  expect_error(check_prob(l), "invalid prob value")
  
})

test_that("check_trials only allows positive integers", {
  
  expect_equal(check_trials(1), TRUE)
  expect_error(check_trials('a'), "invalid trials value")
  expect_error(check_trials('-4'), "invalid trials value")
  
})

test_that("check_success returns appropiate values", {
  
  expect_equal(check_success(2, 5), TRUE)
  expect_error(check_success(4, 2), "success cannot be greater than trials")
  expect_error(check_success('a', 3), "invalid success value")
  
})


context("summary measures")


test_that("aux mean returns a positive number", {
  
  expect_equal(aux_mean(5, 0.5), 2.5)
  expect_gt(aux_mean(5, 0.5), 0)
  expect_type(aux_mean(5, 0.5), 'double')
  
})

test_that("variance_mean returns a positive number", {
  
  expect_equal(aux_variance(5, 0.5), 1.25)
  expect_gt(aux_variance(5, 0.5), 0)
  expect_lt(aux_variance(5, 0.3), 5)
  
})

test_that("aux_mode returns appropriate values", {
  
  expect_equal(aux_mode(6, 0.7), 4)
  expect_gt(aux_mode(6, 1), 0)
  expect_lt(aux_mode(3, 0.2), 1)
  
})

test_that("aux_skewness returns appropriate values", {
  
  expect_equal(aux_skewness(4, 0.5), 0)
  expect_lt(aux_skewness(4, 0.8), 0)
  expect_gt(aux_skewness(4, 0.1), 1)
  
})

test_that("aux_kurtosis returns appropriate values", {
  
  expect_equal(aux_kurtosis(5, 0.2), 0.05)
  expect_lt(aux_kurtosis(5, 0.5), 0)
  expect_gt(aux_kurtosis(5, 0.9), 1)
  
})


context("binomial")

test_that("bin_choose behaves appropriately", {
  
  expect_error(bin_choose(3, 5), "k cannot be greater than n")
  expect_equal(bin_choose(5, 2), 10)
  expect_gt(bin_choose(5, 5), 0)
  
})

test_that("bin_probability behaves appropriately", {
  
  expect_error(bin_probability(4, 2, 0.2), "success cannot be greater than trials")
  expect_lt(bin_probability(2, 8, 0.7), 1)
  expect_length(bin_probability(success = 0:2, trials = 5, prob = 0.5), 3)
  
})

test_that("bin_distribution returns appropriate outputs", {
  
  expect_s3_class(bin_distribution(2, 1), "bindis")
  expect_error(bin_distribution(2, -1), "p has to be a number between 0 and 1")
  expect_length(bin_distribution(5, 0.5), 2)
  
})

test_that("bin_cumulative behaves appropriately", {
  
  expect_error(bin_cumulative(5, 2), "p has to be a number between 0 and 1")
  expect_length(bin_cumulative(5, 0.2), 3)
  expect_s3_class(bin_cumulative(5, 0.5), "bincum")
  
})


