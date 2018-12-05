context("condsim1 test")
ep <- 1e-06
samp <- "function return"

test_that("input", {
  expect_error(condsim1("x"), " * argument 1 must be an integer")
  expect_error(condsim1(-4), " * argument 1 must be positive")
  expect_error(condsim1(c(1, 2, 3, 4)), " * argument 1 cannot have length greater than 1")
  expect_error(condsim1(5.5), " * argument 1 must be an integer")
  expect_error(condsim1(NaN), " * argument 1 cannot include a NaN")
  
})

test_that("output", {
  samp <- condsim1(30)
  expect_that(length(samp), equals(30))
  expect_that(samp, is_a("numeric"))
  set.seed(1234)
  samp <- condsim1(3)
  expect_true(samp[1] >= 1.034005 - ep && samp[1] <= 1.034005 + ep)
  expect_true(samp[2] >= 1.446392 - ep && samp[2] <= 1.446392 + ep)
  expect_true(samp[3] >= 2.008851 - ep && samp[3] <= 2.008851 + ep)
})

test_that("limits", {
  expect_error(condsim1(0), " * argument 1 must be numeric and non-zero")
  set.seed(12345)
  samp <- condsim1(1)
  expect_true(samp[1] >= 2.723746 - ep && samp[1] <= 2.723746 + ep)
  expect_error(condsim1(Inf), " * argument 1 cannot be infinite")
})
