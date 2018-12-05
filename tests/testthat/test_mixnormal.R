context("mixnormal test")
samp <- "function return"
ep <- 1e-06

test_that("input", {
  expect_error(mixnormal(c("x", 5), c(2, 3, 0.5, 0.8, 0.8)), " * argument 1 must be a numeric vector")
  expect_error(mixnormal(c(3, 5), c("x", 3, 0.5, 0.8, 0.8)), " * argument 2 must be a numeric vector")
  expect_error(mixnormal(c(3, NaN), c(2, 3, 0.5, 0.8, 0.8)), " * argument 1 cannot include a NaN")
  expect_error(mixnormal(c(Inf, 5), c(2, 3, 0.5, 0.8, 0.8)), " * argument 1 cannot include an Inf or -Inf")
  expect_error(mixnormal(c(2, 5), c(2, 3, 0.8, 0.8)), " * argument 2 must have 5 elements")
  expect_error(mixnormal(c(1, 5), c(2, 3, Inf, 0.8, 0.8)), " * argument 2 cannot include an Inf or -Inf")
  expect_error(mixnormal(c(3, 5), c(2, NaN, 0.5, 0.8, 0.8)), " * argument 2 cannot include a NaN")
  
})

test_that("output", {
  samp <- mixnormal(c(2, 5), c(2, 3, 1, 0.8, 0.8))
  expect_true(samp[1] >= 2.410043 - ep && samp[1] <= 2.410043 + ep)
  expect_true(samp[2] >= 3.7329366 - ep && samp[2] <= 3.7329366 + ep)
  expect_true(samp[3] >= 1.0305308 - ep && samp[3] <= 1.0305308 + ep)
  expect_true(samp[4] >= 1.4818031 - ep && samp[4] <= 1.4818031 + ep)
  expect_true(samp[5] >= 0.8239189 - ep && samp[5] <= 0.8239189 + ep)
})

test_that("limits", {
  expect_error(mixnormal(c(2, 5), c(2, 3, 0, 0.8, 0.8)), " * element 3 in argument 2 cannot be zero")
  expect_error(mixnormal(c(2, 5), c(2, 3, -1, 0.8, 0.8)), " * element 3 in argument 2 must be positive")
  expect_error(mixnormal(c(2, 5), c(2, 3, 2, 0, 0.8)), " * element 4 in argument 2 cannot be zero")
  expect_error(mixnormal(c(2, 5), c(2, 3, 2, -1, 0.8)), " * element 4 in argument 2 must be positive")
  expect_error(mixnormal(c(2, 5), c(2, 3, 2, 3, 1)), " * element 5 in argument 2 must be greater than 0 and less than 1")
  expect_error(mixnormal(c(2, 5), c(2, 3, 2, 3, 0)), " * element 5 in argument 2 must be greater than 0 and less than 1")
  samp <- mixnormal(c(2, 5), c(2, 3, 2, 3, 0.999999))
  expect_true(samp[1] >= 2.8316366 - ep && samp[1] <= 2.8316366 + ep)
})
