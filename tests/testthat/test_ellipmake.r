context("ellipmake test")

library(visualTest)

test_that("input", {
  expect_error(ellipmake(p = "x"), " * argument 1 must be a number")
  expect_error(ellipmake(p = NaN), " * argument 1 cannot include a NaN")
  expect_error(ellipmake(b = "x"), " * argument 2 must be a number")
  expect_error(ellipmake(b = matrix(c(1, 3, Inf, 1), nrow = 2)), " * argument 2 cannot include an Inf or -Inf")
  expect_error(ellipmake(b = matrix(c(1, 3, NaN, 1), nrow = 2)), " * argument 2 cannot include a NaN")
  expect_error(ellipmake(b = matrix(c(1, 3, -1, 1), nrow = 2)), "argument 2 must have all positive entries")
  expect_error(ellipmake(b = matrix(c(1, 3, 1, 1), nrow = 2)), " * the determinate of argument 2 must be positive")
  expect_error(ellipmake(mu = "x"), " * argument 3 must be a numeric vector")
  expect_error(ellipmake(mu = c(NaN, 3)), " * argument 3 cannot include a NaN")
  expect_error(ellipmake(mu = c(3, Inf)), " * argument 3 cannot include an Inf or -Inf")
})

test_that("output is correct", {
  # make a test picture
  png("../test_img/ellipmaketest.png")
  ellipmake(p = 0.85, b = matrix(c(2, 1, 1, 3), nrow = 2), mu = c(3, 3))
  dev.off()
  finger <- getFingerprint(file = "../test_img/ellipmakekey.png")
  expect_true(isSimilar(file = "../test_img/ellipmaketest.png", finger, threshold = 25))
})

test_that("limits", {
  expect_error(ellipmake(p = 0), " * argument 1 must be greater than 0 and less than 1")
  expect_error(ellipmake(p = 1), " * argument 1 must be greater than 0 and less than 1")
  expect_error(ellipmake(p = -1), " * argument 1 must be greater than 0 and less than 1")
})
