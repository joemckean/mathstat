context("binomci test")
ep <- 1e-06
samp <- "function return"

test_that("input", {
  
  expect_error(binomci("x", 30, 0.4, 0.45, 0.95), " * argument 1 must be a number")
  expect_error(binomci(17, "x", 0.4, 0.45, 0.95), " * argument 2 must be a number")
  expect_error(binomci(17, 30, "x", 0.45, 0.95), " * argument 3 must be a number")
  expect_error(binomci(17, 30, 0.4, "x", 0.95), " * argument 4 must be a number")
  expect_error(binomci(17, 30, 0.4, 0.45, "x"), " * argument 5 must be positive")
  expect_error(binomci(17, 30, 0.4, 0.45, 0.95, "x"), " * argument 6 must be an integer")
  expect_error(binomci(17, 30, 0.4, 0.45, 0.95, eps = "x"), " * argument 7 must be positive")
  expect_error(binomci(17, 30, 0.46, 0.4, 0.95), " * argument 3 must be smaller than argument 4")
  expect_error(binomci(17, 30, 0.4, 0.5, 1), " * argument 5 must be greater than or equal to 0.8192026959")
  expect_error(binomci(NaN, 30, 0.3, 0.34, 0.94), " * argument 1 cannot include a NaN")
  expect_error(binomci(Inf, 30, 0.3, 0.34, 0.94), " * argument 1 cannot include an Inf or -Inf")
  expect_error(binomci(-17, 30, 0.4, 0.45, 0.95), " * argument 1 must be positive")
  expect_error(binomci(17.44, 30, 0.4, 0.45, 0.95), " * argument 1 must be an integer")
  expect_error(binomci(17, NaN, 0.3, 0.34, 0.94), " * argument 2 cannot include a NaN")
  expect_error(binomci(17, Inf, 0.3, 0.34, 0.94), " * argument 2 cannot include an Inf or -Inf")
  expect_error(binomci(17, -30, 0.4, 0.45, 0.95), " * argument 2 must be positive")
  expect_error(binomci(17, 30.81, 0.4, 0.45, 0.95), " * argument 2 must be an integer")
  expect_error(binomci(17, 30, NaN, 0.45, 0.95), " * argument 3 cannot include a NaN")
  expect_error(binomci(17, 30, -0.4, 0.45, 0.95), " * argument 3 must be positive")
  expect_error(binomci(17, 30, 4, 0.45, 0.95), " * argument 3 must be greater than or equal to 0 and less tha")
  expect_error(binomci(17, 30, 0.4, NaN, 0.95), " * argument 4 cannot include a NaN")
  expect_error(binomci(17, 30, 0.4, -0.45, 95), " * argument 4 must be positive")
  expect_error(binomci(17, 30, 0.4, 4, 0.95), " * argument 4 must be greater than or equal to 0 and less tha")
  expect_error(binomci(17, 30, 0.4, 0.45, NaN), " * argument 5 cannot include a NaN")
  expect_error(binomci(17, 30, 0.4, 0.45, -0.95), " * argument 5 must be positive")
  expect_error(binomci(17, 30, 0.4, 0.45, 2), " * argument 5 must be greater than or equal to 0.92861056106562")
  expect_error(binomci(16, 30, 0.3, 0.35, 0.99, NaN), " * argument 6 cannot include a NaN")
  expect_error(binomci(17, 30, 0.4, 0.45, 0.95, -4), " * argument 6 must be positive")
  expect_error(binomci(17, 30, 0.4, 0.45, 0.95, 44.63), " * argument 6 must be an integer")
  expect_error(binomci(16, 30, 0.3, 0.35, 0.99, eps = NaN), " * argument 7 cannot include a NaN")
  expect_error(binomci(17, 30, 0.4, 0.45, 0.95, eps = -0.7), " * argument 7 must be positive")
  
})

test_that("output", {
  samp <- binomci(17, 30, 0.4, 0.45, 0.95)
  expect_is(samp, "list")
  expect_true(samp$solution >= 0.4339417 - ep && samp$solution <= 0.4339417 + ep)
  expect_true(samp$valatsol >= 0.9500042 - ep && samp$valatsol <= 0.9500042 + ep)
  
})

test_that("limits", {
  samp <- binomci(17, 32, 0.4, 0.45, 0.95)
  expect_true(samp$solution >= 0.4031677 - ep && samp$solution <= 0.4031677 + ep)
  expect_true(samp$valatsol >= 0.9500008 - ep && samp$valatsol <= 0.9500008 + ep)
  
  expect_error(binomci(17, 33, 0.4, 0.45, 0.95), " * argument 5 must be greater than or equal to 0.82322902247419")
})
