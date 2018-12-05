context("poisrand.r")

six_decimal_error <- 1e-07

# Test invalid inputs.
test_that("input", {
  expect_error(poisrand("n", 5), "argument 1 must be a number")
  expect_error(poisrand(1000, "lambda"), "argument 2 must be a number")
  expect_error(poisrand(c(1, 2, 3), 5), "argument 1 cannot have length greater than 1")
  expect_error(poisrand(1000, c(1, 2, 3)), "argument 2 cannot have length greater than 1")
  expect_error(poisrand(Inf, 5), "argument 1 cannot include an Inf or -Inf")
  expect_error(poisrand(1000, Inf), "argument 2 cannot include an Inf or -Inf")
  expect_error(poisrand(NaN, 5), "argument 1 cannot include a NaN")
  expect_error(poisrand(1000, NaN), "argument 2 cannot include a NaN")
  expect_error(poisrand(-1, 5), "argument 1 must be positive")
  expect_error(poisrand(1000, -1), "argument 2 must be positive")
  expect_error(poisrand(0, 5), "argument 1 must be numeric and non-zero")
  expect_error(poisrand(1000, 0), "argument 2 must be numeric and non-zero")
})

# Test valid input which result in valid results.

test_that("n=1000, lamba=5", {
  result <- poisrand(1000, 5)
  expect_that(length(result), equals(1000))
  expect_that(abs(5 - mean(result)) < 0.5, is_true())
})

# Test valid input which results in invalid result.
test_that("n=5000000000, lambda=5", {
  expect_that(poisrand(5e+09, 5), throws_error("cannot allocate vector"))
})
