context("Auxillary Function Testing")
library(binomial)

test_that("axuillary mean works as expected", {

  expect_equal(aux_mean(12,0.25), 3)
  expect_type(aux_mean(12,0.25), 'double')
  expect_length(aux_mean(12,0.25), 1)
})

test_that("axuillary variance works as expected", {

  expect_equal(aux_variance(12,0.25), 2.25)
  expect_type(aux_variance(12,0.25), 'double')
  expect_length(aux_variance(12,0.25), 1)
})

test_that("axuillary mode works as expected", {

  expect_equal(aux_mode(12,0.25), 3)
  expect_type(aux_mode(12,0.25), 'double')
  expect_length(aux_mode(12,0.25), 1)
  expect_length(aux_mode(11,0.5), 2)
})

test_that("axuillary skewness works as expected", {

  expect_equal(aux_skewness(12,0.25), 0.3333333)
  expect_type(aux_skewness(12,0.25), 'double')
  expect_length(aux_skewness(12,0.25), 1)
})

test_that("axuillary kurtosis works as expected", {

  expect_equal(aux_kurtosis(12,0.25), -0.0555556)
  expect_type(aux_kurtosis(12,0.25), 'double')
  expect_length(aux_kurtosis(12,0.25), 1)
})

