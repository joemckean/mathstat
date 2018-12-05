context("dloggamma test")
samp <- "function return"
ep <- 1e-07

test_that("input", {
  expect_error(dloggamma(c(4, "x", 4), 2, 3), " * argument 1 must be a numeric vector")
  expect_error(dloggamma(5, "s", 3), " * argument 2 must be a number")
  expect_error(dloggamma(5, 2, "c"), " * argument 3 must be a number")
  expect_error(dloggamma(NaN, 3, 3), " * argument 1 cannot include a NaN")
  expect_error(dloggamma(c(3, 7, 5), NaN, 3), " * argument 2 cannot include a NaN")
  expect_error(dloggamma(c(3, 3, 4), 3, NaN), " * argument 3 cannot include a NaN")
  expect_error(dloggamma(c(3, 5, 3), Inf, 3), " * argument 2 cannot include an Inf or -Inf")
  expect_error(dloggamma(c(3, 5, 3), 3, Inf), " * argument 3 cannot include an Inf or -Inf")
  expect_error(dloggamma(c(2, 4), c(9, 3), 1), " * argument 2 cannot have length greater than 1")
  expect_error(dloggamma(c(2, 4), 1, c(4, 3)), " * argument 3 cannot have length greater than 1")
  expect_error(dloggamma(-1, 4, 4), " * all elements in argument 1 must be greater than 1 and less than Inf")
  expect_error(dloggamma(4, -4, 4), " * argument 2 must be positive")
  expect_error(dloggamma(4, 4, -4), " * argument 3 must be positive")
})

test_that("output", {
  samp <- dloggamma(5, 1, 1)
  expect_is(samp, "numeric")
  expect_true(samp >= 0.04 - ep && samp <= 0.04 + ep)
  samp <- dloggamma(c(3, 8, 9), 2.5, 1.8)
  expect_true(samp[1] >= 0.03607951 - ep && samp[1] <= 0.03607951 + 
    ep)
  expect_true(samp[2] >= 0.0204313 - ep && samp[2] <= 0.0204313 + 
    ep)
  expect_true(samp[3] >= 0.01847638 - ep && samp[3] <= 0.01847638 + 
    ep)
})

test_that("limits", {
  expect_error(dloggamma(c(1, 8), 4, 2), " * all elements in argument 1 must be greater than 1 and less than Inf")
  samp <- dloggamma(1.000001, 4, 2)
  expect_true(samp >= 1.041664e-20 - ep && samp <= 1.041664e-20 + 
    ep)
  expect_error(dloggamma(Inf, 4, 2), " * all elements in argument 1 must be greater than 1 and less than Inf")
  expect_error(dloggamma(5, 0, 1), " * argument 2 must be numeric and non-zero")
  samp <- dloggamma(5, 1e-06, 1)
  expect_true(samp >= 2.485342e-08 - ep && samp <= 2.485342e-08 + 
    ep)
  expect_error(dloggamma(5, 4, 0), " * argument 3 must be numeric and non-zero")
  samp <- dloggamma(5, 4, 1e-06)
  expect_equal(samp, 0)
})
