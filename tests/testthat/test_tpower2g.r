context("tpowerg2 test")
samp <- "function return"
ep <- .0000001
library(visualTest)

test_that("input", {
	expect_error(tpowerg2(Inf, 10, 25, .4, .15),
	             " * argument 1 cannot include an Inf or -Inf")
	expect_error(tpowerg2(NaN, 10, 25, .4, .15),
	             " * argument 1 cannot include a NaN")
	expect_error(tpowerg2("x", 10, 25, .4, .15),
	             " * argument 1 must be a number")
	expect_error(tpowerg2(c(3,2,1), 10, 25, .4, .15),
	             " * argument 1 cannot have length greater than 1")
	expect_error(tpowerg2(20, NaN, 25, .4, .15),
	             " * argument 2 cannot include a NaN")
	expect_error(tpowerg2(25, "x", 25, .4, .15),
	             " * argument 2 must be a number")
	expect_error(tpowerg2(25, c(10, 3, 2), 25, .4, .15),
	             " * argument 2 cannot have length greater than 1")
  expect_error(tpowerg2(25, 10, NaN, .4, .15),
	             " * argument 3 cannot include a NaN")
	expect_error(tpowerg2(25, 10, 5.5, .4, .15),
	             " * argument 3 must be an integer")
	expect_error(tpowerg2(25, 10, "x", .4, .15),
	             " * argument 3 must be an integer")
	expect_error(tpowerg2(25, 10, 25, NaN, .15),
	             " * argument 4 cannot include a NaN")
	expect_error(tpowerg2(25, 10, 25, Inf, .15),
	             " * argument 4 cannot include an Inf or -Inf")
	expect_error(tpowerg2(25, 10, 25, "x", .15),
	             " * argument 4 must be a number")
	expect_error(tpowerg2(25, 10, 25, .4, NaN),
	             " * argument 5 cannot include a NaN")
	expect_error(tpowerg2(25, 10, 25, .4, Inf),
	             " * argument 5 cannot include an Inf or -Inf")
	expect_error(tpowerg2(25, 10, 25, .4, "x"),
	             " * argument 5 must be a number")
})

test_that("output", {
	png("../test_img/tpowerg2test.png")
	samp <- tpowerg2(10, 10, 10, .4)
	dev.off()
  finger <- getFingerprint(file = "../test_img/tpowerg2test.png")
	expect_true(isSimilar(file = "../test_img/tpowerg2key.png",
						  finger,
						  threshold=10))
	expect_true(samp[1] >= 0.9989441 - ep && samp[1] <= 0.9989441 + ep)
	expect_true(samp[71] >= 0.8228882 - ep && samp[71] <= 0.8228882 + ep)
	expect_true(samp[127] >= 0.4000545 - ep && samp[127] <= 0.4000545 + ep)
})

test_that("limits", {
	expect_error(tpowerg2(20, 0, 25, .4, .15),
	             " * argument 2 must be greater than 0 and less than Inf")
  expect_error(tpowerg2(20, Inf, 25, .4, .15),
	             " * argument 2 must be greater than 0 and less than Inf")
	expect_error(tpowerg2(20, 10, 1, .4, .15),
	             " * argument 3 must be greater than 1 and less than Inf")
  expect_error(tpowerg2(20, 10, Inf, .4, .15),
	             " * argument 3 must be greater than 1 and less than Inf")
	expect_error(tpowerg2(20, 10, 25, 0, .15),
	             " * argument 4 must be greater than 0 and less than 1")
  expect_error(tpowerg2(20, 10, 25, 1, .15),
	             " * argument 4 must be greater than 0 and less than 1")
	expect_error(tpowerg2(20, 10, 25, .4, 0),
	             " * argument 5 must be greater than 0 and less than Inf")
})
