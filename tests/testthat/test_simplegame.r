context("simplegame.r")

six_decimal_error <- 1e-07

# Test invalid inputs.
test_that("input", {
  expect_error(simplegame("amtpaid"), "argument 1 must be a number")
  expect_error(simplegame(Inf), "argument 1 cannot include an Inf or -Inf")
  expect_error(simplegame(NaN), "argument 1 cannot include a NaN")
  expect_error(simplegame(-1), "argument 1 must be positive")
  expect_error(simplegame(0), "argument 1 must be numeric and non-zero")
})

# Test valid input which yield valid results.

test_that("amtpaid=5", {
  result <- simplegame(5)
  expect_that(length(result), equals(1))
})
