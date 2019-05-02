context("Checker Function Testing")
library(binomial)

test_that("check_prob works as expected", {

  expect_equal(check_prob(.5), TRUE)
  expect_error(check_prob(1500))
  expect_type(check_prob(.5), 'logical')
  expect_length(check_prob(.5), 1)
})

test_that("check_trials works as expected", {
  x = 2

  expect_error(check_trials(74.5))
  expect_length(check_trials(x), 1)
  expect_type(check_trials(x), 'logical')
})

test_that("check_success works as expected", {
  x = c(2, 4, 6, 8, 10, 12)

  expect_equal(check_success(success = x, trials = 20), TRUE)
  expect_error(check_success(success = x, trials = 10))
  expect_length(check_success(success = x, trials = 20), 1)
  expect_type(check_success(success = x, trials = 20), 'logical')
})
