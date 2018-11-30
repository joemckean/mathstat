context("rscn test")
samp <- "function return"
ep <- .000001

test_that("input", {
  expect_error(rscn("x", .4, 2, 9), "argument 1 must be a number")
  expect_error(rscn(6, "x", 2, 9), "argument 2 must be a number")
  expect_error(rscn(6, .3, "x", 9), "argument 3 must be a number")
  expect_error(rscn(6, .3, 2, "x"), "argument 4 must be a number")
  expect_error(rscn(NaN, .3, 2, 9), "argument 1 cannot include a NaN")
  expect_error(rscn(Inf, .3, 2, 9), "argument 1 cannot include an Inf or -Inf")
  expect_error(rscn(c(3, 2, 4), .3, 2, 9), "argument 1 cannot have length greater than 1")
  expect_error(rscn(3.4, .3, 2, 9), "argument 1 must be an integer")
  expect_error(rscn(3, NaN, 3, 2), "argument 2 cannot include a NaN")
  expect_error(rscn(3, Inf, 3, 2), "argument 2 cannot include an Inf or -Inf")
  expect_error(rscn(3, c(4, 3, 2), 3, 2), "argument 2 cannot have length greater than 1")
  expect_error(rscn(3, .4, NaN, 2), "argument 3 cannot include a NaN")
  expect_error(rscn(3, .4, Inf, 2), "argument 3 cannot include an Inf or -Inf")
  expect_error(rscn(3, .2, c(3, 2), 2), "argument 3 cannot have length greater than 1")
  expect_error(rscn(3, .4, 2, NaN), "argument 4 cannot include a NaN")
  expect_error(rscn(3, .4, 2, Inf), "argument 4 cannot include an Inf or -Inf")
  expect_error(rscn(3, .4, 2, c(3,2,3)), "argument 4 cannot have length greater than 1")
})

test_that("output", {
  samp <- rscn(5, .5, 3, 14)
  expect_that(samp, is_a("numeric"))
  samp <- rscn(55, .4, 2.3, 26)
  expect_that(length(samp), equals(55))
  set.seed(1234)
  samp <- rscn(2, .7, 3, 4)
  expect_true(samp[1] >= 7.253324 - ep && samp[1] <= 7.253324 + ep)
  expect_true(samp[2] >= -3.037093 - ep && samp[2] <= -3.037093 + ep)
})

test_that("limits", {
  expect_error(rscn(0, .3, 2, 9), "argument 1 must be numeric and non-zero")
  expect_error(rscn(-1, .3, 2, 9), "argument 1 must be positive")
  expect_error(rscn(3, -.00001, 3, 2),
               "argument 2 must be greater than or equal to 0 and les")
  expect_error(rscn(3, .5, 0.9999999, 2),
               "argument 3 must be greater than or equal to 1 and les")
})
