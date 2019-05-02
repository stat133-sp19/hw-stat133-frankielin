context("Binomial Main Function Testing")
library(binomial)

test_that("bin_choose works as expected", {

  expect_error(bin_choose(n = 12,k = 0.25))
  expect_error(bin_choose(n = 1, k = 50))
  expect_error(bin_choose(n = 12, k = NA))
  expect_error(bin_choose(n = NA, k = 12))
  expect_error(bin_choose(n = 1,k = 50))
  expect_type(bin_choose(n = 5, k = 5), "double")
  expect_equal(bin_choose(n = 5, k = 5), 1)
})

test_that("bin_probability works as expected", {

  expect_error(bin_probability(success = 5, trials = 10, prob = 10))
  expect_error(bin_probability(success = 10, trials = 5, prob = 0.25))
  expect_error(bin_probability(success = 5, trials = 10, prob = -0.25))
  expect_type(bin_probability(success = 5, trials = 10, prob = 0.25), "double")
  expect_equal(bin_probability(success = 5, trials = 10, prob = 0.25), 0.0583992)
})

test_that("bin_distribution works as expected", {

  expect_error(bin_distribution(trials = 10, prob = 10))
  expect_error(bin_distribution(trials = 10, prob = -0.25))
  expect_length(bin_distribution(trials = 10, prob = 0.25), 2)
  expect_equal(nrow(bin_distribution(trials = 10, prob = 0.25)), 11)
  expect_type(bin_distribution(trials = 10, prob = 0.25), 'list')
  expect_equal(class(bin_distribution(trials = 10, prob = 0.25)), c("bindis", "data.frame"))
})

test_that("bin_cumulative works as expected", {

  expect_error(bin_cumulative(trials = 10, prob = 10))
  expect_error(bin_cumulative(trials = 10, prob = -0.25))
  expect_length(bin_cumulative(trials = 10, prob = 0.25), 3)
  expect_equal(nrow(bin_cumulative(trials = 10, prob = 0.25)), 11)
  expect_type(bin_cumulative(trials = 10, prob = 0.25), 'list')
  expect_equal(class(bin_cumulative(trials = 10, prob = 0.25)), c("bincum", "data.frame"))
})






