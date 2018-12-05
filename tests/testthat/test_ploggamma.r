context("ploggamma.r")

six_decimal_error <- 1e-07

# Test invalid inputs.
test_that("input", {
  expect_error(ploggamma("x", 1, 1), "argument 1 must be a number")
  expect_error(ploggamma(c(10, 11, 12), "alpha", 1), "argument 2 must be a number")
  expect_error(ploggamma(c(10, 11, 12), 1, "beta"), "argument 3 must be a number")
  
  expect_error(ploggamma(Inf, 1, 1), "argument 1 cannot include an Inf or -Inf")
  expect_error(ploggamma(c(10, 11, 12), Inf, 1), "argument 2 cannot include an Inf or -Inf")
  expect_error(ploggamma(c(10, 11, 12), 1, Inf), "argument 3 cannot include an Inf or -Inf")
  
  expect_error(ploggamma(NaN, 1, 1), "argument 1 cannot include a NaN")
  expect_error(ploggamma(c(10, 11, 12), NaN, 1), "argument 2 cannot include a NaN")
  expect_error(ploggamma(c(10, 11, 12), 1, NaN), "argument 3 cannot include a NaN")
  
  expect_error(ploggamma(c(10, 11, 12), -1, 1), "argument 2 must be positive")
  expect_error(ploggamma(c(10, 11, 12), 1, -1), "argument 3 must be positive")
  
  expect_error(ploggamma(c(0, 11, 12), 1, 1), "all elements in argument 1 must be greater than or equal to 1")
  expect_error(ploggamma(c(10, 11, 12), 1, 0), "argument 3 must be numeric and non-zero")
})

# Test valid input which yield valid results.

test_that("x=c(10,11,12), alpha=0.5, beta=0.25", {
  x <- c(10, 11, 12)
  result <- ploggamma(x, 1, 1)
  expected <- c(0.9, 0.909090909090909, 0.916666666666667)
  
  expect_that(length(result), equals(length(x)))
  expect_that(all(abs(expected - result) < six_decimal_error), is_true())
})
