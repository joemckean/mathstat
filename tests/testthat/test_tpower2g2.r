context("tpower2g2 test")
samp <- "function return"
ep <- .000001

test_that("input", {
	expect_error(tpower2g2("x", 3, 2, .6, .02),
               "argument 1 must be a number")
  expect_error(tpower2g2(3, "x", 2, .6, .02),
						   "argument 2 must be a number")
  expect_error(tpower2g2(3, 3, "x", .6, .02),
 						   "argument 3 must be a number")
  expect_error(tpower2g2(3, 3, 2, "x", .02),
               "* argument 4 must be numeric and non-zero")
  expect_error(tpower2g2(3, 3, 2, .6, "x"),
               "argument 5 must be a number")
	expect_error(tpower2g2(Inf, 3, 2, .6, .04),
               "argument 1 cannot include an Inf or -Inf")
  expect_error(tpower2g2(NaN, 3, 2, .6, .04),
               "argument 1 cannot include a NaN")
  expect_error(tpower2g2(3, Inf, 2, .6, .04),
               "argument 2 cannot include an Inf or -Inf")
  expect_error(tpower2g2(3, NaN, 2, .6, .04),
               "argument 2 cannot include a NaN")
  expect_error(tpower2g2(3, 3, Inf, .6, .04),
               "argument 3 cannot include an Inf or -Inf")
  expect_error(tpower2g2(3, 3, NaN, .6, .04),
               "argument 3 cannot include a NaN")
  expect_error(tpower2g2(3, 3, 2, Inf, .04),
               "argument 4 cannot include an Inf or -Inf")
  expect_error(tpower2g2(3, 3, 3, NaN, .04),
               "argument 4 cannot include a NaN")
  expect_error(tpower2g2(3, 5, 2, .3, Inf),
               "argument 5 cannot include an Inf or -Inf")
  expect_error(tpower2g2(3, 3, 3, .4, NaN),
               "argument 5 cannot include a NaN")
})

test_that("output", {
  samp <- tpower2g2(29, 34, 5, 10, .05)
	expect_is(samp, "numeric")
	expect_true(samp >= 0.6221714 - ep && samp <= 0.6221714 +ep)
})

test_that("limits", {
  expect_error(tpower2g2(-1, 4, 3, .2, .03),
               "argument 1 must be positive")
  expect_error(tpower2g2(4, -1, 2, .3, .01),
               "argument 2 must be positive")
	expect_error(tpower2g2(3, 3, 2, 0, .04),
               "argument 4 must be numeric and non-zero")
 	expect_error(tpower2g2(3, 2, 3, -1, .04),
               "argument 4 must be positive")
	expect_error(tpower2g2(2, 3, 5, .2, 1),
               "argument 5 must be greater than 0 and less than 1")
  expect_error(tpower2g2(2, 3, 5, .2, 0),
               "argument 5 must be greater than 0 and less than 1")
})
