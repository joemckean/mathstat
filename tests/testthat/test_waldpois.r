context("waldpois test")
samp <- "function return"
ep <- 1e-05

test_that("input", {
  expect_error(waldpois(c(4, "x", 1), 5), " * argument 1 must be a numeric vector")
  expect_error(waldpois(5, "x"), " * argument 2 must be a numeric vector")
  expect_error(waldpois(c(3, NaN, 9, -3), 2), " * argument 1 cannot include a NaN")
  expect_error(waldpois(2, c(2, -1, NaN)), " * argument 2 cannot include a NaN")
  expect_error(waldpois(c(3, Inf, 9, -3), 2), " * argument 1 cannot include an Inf or -Inf")
  expect_error(waldpois(2, c(2, -1, Inf)), " * argument 2 cannot include an Inf or -Inf")
  expect_error(waldpois(c(5, -5, 8, -8), 7), " * argument 1 must have a mean that is not zero")
})

test_that("output", {
  samp <- waldpois(c(3, 4.93, -11, 67), c(-8, 27.5))
  expect_true(samp[1] >= 143.94752 - ep && samp[1] <= 143.94752 + ep)
  expect_true(samp[2] >= 33.19951 - ep && samp[2] <= 33.19951 + ep)
})
