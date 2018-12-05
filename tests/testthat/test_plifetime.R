# Test for plifetime function Returns: An error message if any tests fail
context("plifetime")

test_that("edge cases", {
  expect_equal(plifetime(1e+45, 1e+55, 1e+45, 1e+55), 0)
})

test_that("input", {
  # Check for non-numeric values
  expect_error(plifetime("test", 2, 1, 2), "argument 1 must be a number")
  expect_error(plifetime(1, "test", 1, 2), "argument 2 must be a number")
  expect_error(plifetime(1, 2, "test", 2), "argument 3 must be a number")
  expect_error(plifetime(1, 2, 1, "test"), "argument 4 must be a number")
  # Check for NA values
  expect_error(plifetime(NA, 2, 1, 2), "argument 1 must be a number")
  expect_error(plifetime(1, NA, 1, 2), "argument 2 must be a number")
  expect_error(plifetime(1, 2, NA, 3), "argument 3 must be a number")
  expect_error(plifetime(1, 2, 1, NA), "argument 4 must be a number")
  # Check for NaN values
  expect_error(plifetime(NaN, 2, 1, 2), "argument 1 cannot include a NaN")
  expect_error(plifetime(1, NaN, 1, 2), "argument 2 cannot include a NaN")
  expect_error(plifetime(1, 2, NaN, 2), "argument 3 cannot include a NaN")
  expect_error(plifetime(1, 2, 1, NaN), "argument 4 cannot include a NaN")
})

test_that("output", {
  expect_equal(round(plifetime(1, 2, 1, 2), 5), 0.12219)
  expect_equal(plifetime(sqrt(2)/2, sqrt(2)/2, Inf, Inf), 0)
  expect_equal(length(plifetime(1, 2, 1, 2)), 1)
  expect_equal(is.numeric(plifetime(1, 2, 1, 2)), TRUE)
  expect_equal(is.vector(plifetime(1, 2, 1, 2)), TRUE)
})


