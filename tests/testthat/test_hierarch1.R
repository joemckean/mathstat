# Test for hierarch1 function Returns: An error message if an tests fail

context("hierarch1")

test_that("edge cases", {
  # Check for nsims
  expect_error(hierarch1(nsims = Inf), "argument 1 cannot include an Inf or -Inf")
  expect_error(hierarch1(nsims = -Inf), "argument 1 cannot include an Inf or -Inf")
  expect_error(hierarch1(nsims = -1), "argument 1 must be positive")
  # Check for x
  expect_error(hierarch1(nsims = 100, x = Inf), "argument 2 cannot include an Inf or -Inf")
  expect_error(hierarch1(nsims = 100, x = -Inf), "argument 2 cannot include an Inf or -Inf")
  expect_error(hierarch1(nsims = 100, x = -1), "argument 2 must be positive")
  # Check for tau
  expect_error(hierarch1(nsims = 100, x = 1, tau = Inf), "argument 3 cannot include an Inf or -Inf")
  expect_error(hierarch1(nsims = 100, x = 1, tau = -Inf), "argument 3 cannot include an Inf or -Inf")
  expect_error(hierarch1(nsims = 100, x = 1, tau = -1), "argument 3 must be positive")
  # Check for kstart
  expect_error(hierarch1(nsims = 100, x = 1, kstart = Inf), "argument 4 cannot include an Inf or -Inf")
  expect_error(hierarch1(nsims = 100, x = 1, kstart = -Inf), "argument 4 cannot include an Inf or -Inf")
  expect_error(hierarch1(nsims = 100, x = 1, kstart = -1), "argument 4 must be positive")
})

test_that("input", {
  # Check for nsims
  expect_error(hierarch1(nsims = NA), "argument 1 must be numeric and non-zero")
  expect_error(hierarch1(nsims = NA), "argument 1 must be a number")
  expect_error(hierarch1(nsims = "test"), "argument 1 must be numeric and non-zero")
  expect_error(hierarch1(nsims = "test"), "argument 1 must be positive")
  expect_error(hierarch1(nsims = "test"), "argument 1 must be a number")
  # Check for x
  expect_error(hierarch1(nsims = 50, x = NA), "argument 2 must be numeric and non-zero")
  expect_error(hierarch1(nsims = 50, x = NA), "argument 2 must be positive")
  expect_error(hierarch1(nsims = 50, x = NA), "argument 2 must be a number")
  expect_error(hierarch1(nsims = 50, x = "test"), "argument 2 must be numeric and non-zero")
  expect_error(hierarch1(nsims = 50, x = "test"), "argument 2 must be positive")
  expect_error(hierarch1(nsims = 50, x = "test"), "argument 2 must be a number")
  # Check for tau
  expect_error(hierarch1(nsims = 50, x = 1, tau = NA), "argument 3 must be numeric and non-zero")
  expect_error(hierarch1(nsims = 50, x = 1, tau = NA), "argument 3 must be positive")
  expect_error(hierarch1(nsims = 50, x = 1, tau = NA), "argument 3 must be a number")
  expect_error(hierarch1(nsims = 50, x = 1, tau = "test"), "argument 3 must be numeric and non-zero")
  expect_error(hierarch1(nsims = 50, x = 1, tau = "test"), "argument 3 must be positive")
  expect_error(hierarch1(nsims = 50, x = 1, tau = "test"), "argument 3 must be a number")
  # Check for kstart
  expect_error(hierarch1(nsims = 50, x = 1, kstart = NA), "argument 4 must be numeric and non-zero")
  expect_error(hierarch1(nsims = 50, x = 1, kstart = NA), "argument 4 must be positive")
  expect_error(hierarch1(nsims = 50, x = 1, kstart = NA), "argument 4 must be a number")
  expect_error(hierarch1(nsims = 50, x = 1, kstart = "test"), "argument 4 must be numeric and non-zero")
  expect_error(hierarch1(nsims = 50, x = 1, kstart = "test"), "argument 4 must be positive")
  expect_error(hierarch1(nsims = 50, x = 1, kstart = "test"), "argument 4 must be a number")
})

test_that("output", {
  # Check class output name
  expect_equal(attributes(hierarch1(nsims = 30, x = 6, tau = 0.05, kstart = 4))$names, c("clambda", 
    "cb", "gibbslambda", "gibbsb"))
  # Check class output is.numeric
  expect_true(is.numeric(hierarch1(nsims = 30, x = 6, tau = 0.05, kstart = 4)$clambda))
  expect_true(is.numeric(hierarch1(nsims = 30, x = 6, tau = 0.05, kstart = 4)$cb))
  expect_true(is.numeric(hierarch1(nsims = 30, x = 6, tau = 0.05, kstart = 4)$gibbslambda))
  expect_true(is.numeric(hierarch1(nsims = 30, x = 6, tau = 0.05, kstart = 4)$gibbsb))
  # Check class output length
  expect_equal(length(hierarch1(nsims = 30, x = 6, tau = 0.05, kstart = 4)$clambda), 34)
  expect_equal(length(hierarch1(nsims = 30, x = 6, tau = 0.05, kstart = 4)$cb), 34)
  expect_equal(length(hierarch1(nsims = 30, x = 6, tau = 0.05, kstart = 4)$gibbslambda), 30)
  expect_equal(length(hierarch1(nsims = 30, x = 6, tau = 0.05, kstart = 4)$gibbsb), 30)
})


