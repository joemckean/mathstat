# Test for aresimcn function Returns: An error message if any tests fail

context("rcn")

test_that("edge cases", {
  expect_error(rcn(Inf), "argument 1 cannot include an Inf or -Inf")
  expect_error(rcn(n = -1), "argument 1 must be positive")
  expect_error(rcn(n = 50, eps = -1))
  expect_error(rcn(n = 50, eps = 1.01))
  expect_error(rcn(n = 50, eps = Inf), "argument 2 cannot include an Inf or -Inf")
  expect_error(rcn(n = 50, sigma_c = -1), "argument 3 must be positive")
  expect_error(rcn(n = 50, sigma_c = Inf), "argument 3 cannot include an Inf or -Inf")
  
})

test_that("input", {
  # Checking invalid input for n
  expect_error(rcn(n = 0), "argument 1 must be numeric and non-zero")
  expect_error(rcn(n = NA), "argument 1 must be a number")
  expect_error(rcn(n = NA), "argument 1 must be numeric and non-zero")
  expect_error(rcn(n = NA), "argument 1 must be positive")
  # Checking invalid input for eps
  expect_error(rcn(n = 50, eps = NA), "argument 2 must be a number")
  expect_error(rcn(n = 50, eps = NaN), "argument 2 cannot include a NaN")
  # Checking invalid input for vc
  expect_error(rcn(n = 50, sigma_c = NA), "argument 3 must be positive")
  expect_error(rcn(n = 50, sigma_c = NA), "argument 3 must be a number")
})

test_that("output", {
  expect_equal(is.numeric(rcn(30, 0.25, 3)), TRUE)
  expect_equal(is.vector(rcn(30, 0.25, 3)), TRUE)
  expect_equal(length(rcn(30, 0.25, 3)), 30)
})


