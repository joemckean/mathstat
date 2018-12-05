context("pcn.r")

six_decimal_error <- 1e-07

# Test invalid inputs.
test_that("input", {
  expect_error(pcn("w", 0.5, 0.25), "argument 1 must be a number")
  expect_error(pcn(10, "eps", 0.25), "argument 2 must be a number")
  expect_error(pcn(10, 0.5, "sigma_c"), "argument 3 must be a number")
  
  expect_error(pcn(c(1, 2, 3), 0.5, 0.25), "argument 1 cannot have length greater than 1")
  expect_error(pcn(10, c(1, 2, 3), 0.25), "argument 2 cannot have length greater than 1")
  expect_error(pcn(10, 0.5, c(1, 2, 3)), "argument 3 cannot have length greater than 1")
  
  expect_error(pcn(Inf, 0.5, 0.25), "argument 1 cannot include an Inf or -Inf")
  expect_error(pcn(10, Inf, 0.25), "argument 2 cannot include an Inf or -Inf")
  expect_error(pcn(10, 0.5, Inf), "argument 3 cannot include an Inf or -Inf")
  
  expect_error(pcn(NaN, 0.5, 0.25), "argument 1 cannot include a NaN")
  expect_error(pcn(10, NaN, 0.25), "argument 2 cannot include a NaN")
  expect_error(pcn(10, 0.5, NaN), "argument 3 cannot include a NaN")
  
  
  expect_error(pcn(10, 0.5, -0.25), "argument 3 must be positive")
  
  expect_error(pcn(10, 0.5, 0), "argument 3 must be numeric and non-zero")
})

# Test valid input which result in valid results.

test_that("w=10, eps=0.5, sigma_c=0.25", {
  result <- pcn(10, 0.5, 0.25)
  expect_that(length(result), equals(1))
  expect_that(abs(1 - result) < six_decimal_error, is_true())
})
