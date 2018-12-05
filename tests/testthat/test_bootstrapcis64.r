context("bootstrapcis64 test")
samp <- "function return"
ep <- 1e-06

test_that("input", {
  expect_error(bootstrapcis64("x", 6), " * argument 1 must be a numeric vector")
  expect_error(bootstrapcis64(c(1, 3, 3.4), "x"), " * argument 2 must be a number")
  expect_error(bootstrapcis64(c(1, 3, 3.4), 6, "x"), " * argument 3 must be an integer")
  expect_error(bootstrapcis64(c(1, 3, 3.4), 6, alp2 = "x"), " * argument 4 must be greater than 0 and less than 1")
  expect_error(bootstrapcis64(Inf, 6), " * argument 1 must have length greater than 1")
  expect_error(bootstrapcis64(NaN, 6), " * argument 1 must have length greater than 1")
  expect_error(bootstrapcis64(c(2, 3, 4), c(4, 3, 2)), " * argument 2 cannot have length greater than 1")
  expect_error(bootstrapcis64(c(2, 3, 4), NaN), " * argument 2 cannot include a NaN")
  expect_error(bootstrapcis64(c(3, 4, 5), 6, c(3, 4)), " * argument 3 cannot have length greater than 1")
  expect_error(bootstrapcis64(c(3, 3, 2), 5, Inf), " * argument 3 cannot be infinite")
  expect_error(bootstrapcis64(c(3, 3, 2), 5, NaN), " * argument 3 cannot include a NaN")
  expect_error(bootstrapcis64(c(3, 2, 5), 8, alp2 = NaN), " * argument 4 cannot include a NaN")
  expect_error(bootstrapcis64(c(2, 4, 5), 2, alp2 = 0), " * argument 4 must be greater than 0 and less than 1")
})

test_that("output", {
  samp <- bootstrapcis64(c(4, 31, 9.2, -9), 32)
  expect_true(samp$estimate >= 0.9460584 - ep && samp$estimate <= 0.9460584 + ep)
  expect_equal(samp$upperbound, 1)
})

test_that("limits", {
  expect_error(bootstrapcis64(c(3, 2, 4), 5, 0), " * argument 3 must be numeric and non-zero")
  expect_error(bootstrapcis64(c(3, 3, 2), 5, -83), " * argument 3 must be positive")
})
