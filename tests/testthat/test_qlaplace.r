context("qlaplace test")
ep <- .0000001
samp <- "function return"

test_that("input", {
	expect_error(qlaplace("x"),
               " * argument 1 must be a numeric vector")
	expect_error(qlaplace(c(.4, NaN)),
	             " * argument 1 cannot include a NaN")
})

test_that("output", {
  expect_equal(qlaplace(.5), 0)
	samp <- qlaplace(c(.9, .2, .7, .4))
	expect_true(samp[1] >= -0.9162907 - ep && samp[1] <= -0.9162907 + ep)
	expect_true(samp[2] >= -0.2231436 - ep && samp[2] <= -0.2231436 + ep)
	expect_true(samp[3] >= 1.6094379 - ep && samp[3] <= 1.6094379 + ep)
	expect_true(samp[4] >= 0.5108256 - ep && samp[4] <= 0.5108256 + ep)
})

test_that("limits", {
  expect_equal(qlaplace(0), -Inf)
	expect_equal(qlaplace(1), Inf)
	expect_error(qlaplace(-.00000001),
               " * all elements in argument 1 must be greater than or equal t")
	expect_error(qlaplace(1.0000000001),
               " * all elements in argument 1 must be greater than or equal t")
})
