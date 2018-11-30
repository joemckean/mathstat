context('percentciboot test')
samp <- "function return"
ep <- .00001

test_that("input", {
  expect_error(percentciboot("x", 20),
               " * argument 1 must be a numeric vector")
  expect_error(percentciboot(c(3, 2, 4.3), "x"),
               " * argument 2 must be an integer")
  expect_error(percentciboot(c(3, 5, 2), 20, "x"),
               " * argument 3 must be greater than 0 and less than 1")
	expect_error(percentciboot(Inf, 20),
	             " * argument 1 cannot include an Inf or -Inf")
	expect_error(percentciboot(NaN, 20),
               " * argument 1 cannot include a NaN")
	expect_error(percentciboot(c(4, 3, 2), c(3, 2)),
               " * argument 2 cannot have length greater than 1")
	expect_error(percentciboot(c(8,2,9), NaN),
               " * argument 2 cannot include a NaN")
	expect_error(percentciboot(c(9.4,8,2), 30.8),
               " * argument 2 must be an integer")
	expect_error(percentciboot(c(9,4,3), Inf),
               " * argument 2 cannot be infinite")
	expect_error(percentciboot(c(5,8,2), 30, NaN),
               " * argument 3 cannot include a NaN")

})

test_that("output", {
	set.seed(1234)
	samp <- percentciboot(c(4, 3, 5, 30, 18, 5), 20, .2)
	expect_is(samp, 'list')
	expect_is(samp['theta'], 'list')
	expect_true(samp$theta >= 10.83333 - ep && samp$theta <= 10.83333 + ep)
	expect_is(samp['lower'], 'list')
	expect_true(samp$lower >= 3.5 - ep && samp$lower <= 3.5 + ep)
	expect_is(samp['upper'], 'list')
	expect_true(samp$upper >= 19.16667 - ep && samp$upper <= 19.16667 + ep)
	expect_is(samp['thetastar'], 'list')
	expect_true(samp$thetastar[4] >= 4.166667 - ep && samp$thetastar[4] <= 4.166667 +ep)
  expect_true(length(samp$thetastar)==20)
})

test_that("limits", {
	expect_error(percentciboot(c(3,9,3), 19),
	             " * argument 2 must be greater than or equal to 20 and le")
	expect_error(percentciboot(c(5,39,0), 30, 0),
						   " * argument 3 must be greater than 0 and less than 1")
	expect_error(percentciboot(c(5,39,0), 30, 1),
						   " * argument 3 must be greater than 0 and less than 1")
})
