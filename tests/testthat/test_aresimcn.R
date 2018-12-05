# Test for aresimcn function Returns: An error message if any tests fail

context("aresimcn")

test_that("edge cases", {
  expect_error(aresimcn(Inf), "argument 1 cannot include an Inf or -Inf")
  expect_error(aresimcn(n = -1), "argument 1 must be positive")
  expect_error(aresimcn(n = 50, nsims = Inf), "argument 2 cannot include an Inf or -Inf")
  expect_error(aresimcn(n = 50, nsims = -1), "argument 2 must be positive")
  expect_error(aresimcn(n = 50, eps = -1), "input argument 'eps' must be between zero and one")
  expect_error(aresimcn(n = 50, eps = 1), "input argument 'eps' must be between zero and one")
  expect_error(aresimcn(n = 50, nsims = 100, eps = Inf), "argument 3 cannot include an Inf or -Inf")
  expect_error(aresimcn(n = 50, nsims = 100, vc = -1), "argument 4 must be positive")
  expect_error(aresimcn(n = 50, vc = Inf), "argument 4 cannot include an Inf or -Inf")
  
})

test_that("input", {
  # Checking invalid input for n
  expect_error(aresimcn(n = 0), "argument 1 must be numeric and non-zero")
  expect_error(aresimcn(n = NA), "argument 1 must be a number")
  expect_error(aresimcn(n = NA), "argument 1 must be numeric and non-zero")
  expect_error(aresimcn(n = NA), "argument 1 must be positive")
  # Checking invalid input for nsims
  expect_error(aresimcn(nsims = NA), "argument 2 must be a number")
  expect_error(aresimcn(nsims = NA), "argument 2 must be numeric and non-zero")
  expect_error(aresimcn(nsims = NA), "argument 2 must be positive")
  # Checking invalid input for eps
  expect_error(aresimcn(n = 50, eps = NA), "argument 3 must be a number")
  # Checking invalid input for vc
  expect_error(aresimcn(n = 50, vc = NA), "argument 4 must be positive")
  expect_error(aresimcn(n = 50, vc = NA), "argument 4 must be a number")
})

test_that("output", {
  expect_equal(is.numeric(aresimcn(30, 100, 0.25, 3)), TRUE)
  expect_equal(is.vector(aresimcn(30, 100, 0.25, 3)), TRUE)
  expect_equal(length(aresimcn(30, 100, 0.25, 3)), 1)
})


