# Test for mses function Returns: An error message if any tests fail

context("mses")

set.seed(112)

sample1 <- rnorm(30)
theta <- var(sample1)

test_that("edge cases", {
  expect_error(mses(Inf), "argument 1 must have length greater than 1")
  expect_error(mses(Inf), "argument 1 cannot include an Inf or -Inf")
  expect_error(mses(-Inf), "argument 1 must have length greater than 1")
  expect_error(mses(-Inf), "argument 1 cannot include an Inf or -Inf")
  expect_error(mses(sample1, Inf), "argument 2 cannot be infinite")
  expect_error(mses(sample1, -Inf), "argument 2 cannot be infinite")
})
test_that("input", {
  expect_error(mses(as.matrix(sample1)), "argument 1 must be a numeric vector")
  expect_error(mses(c("a", "b", "c")), "argument 1 must be a number")
  expect_error(mses(c("a", "b", "c")), "argument 1 must be a numeric vector")
  # Check for NaN values: x
  expect_error(mses(c(NaN, 2, 3, 4)), "argument 1 cannot include a NaN")
  expect_error(mses(c(1, NaN, 3, 4)), "argument 1 cannot include a NaN")
  expect_error(mses(c(1, 2, NaN, 4)), "argument 1 cannot include a NaN")
  expect_error(mses(c(1, 2, 3, NaN)), "argument 1 cannot include a NaN")
  # Check for NA values: theta0
  expect_error(mses(sample1, NA), "argument 2 must be a number")
  # Check for NaN values: theta0
  expect_error(mses(sample1, NaN), "argument 2 cannot include a NaN")
})

test_that("output", {
  expect_equal(is.numeric(mses(sample1, theta)), TRUE)
  expect_equal(length(mses(sample1)), 1)
})


