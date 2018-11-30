context("boottestonemed test")
samp <- "function return"


test_that("input", {
  expect_error(boottestonemed("x", .5, 4),
	             " * argument 1 must be a numeric vector")
  expect_error(boottestonemed(c(1,2,3), "x", 5),
	             " * argument 2 must be a number")
  expect_error(boottestonemed(c(1,3,4), .5, "x"),
	             " * argument 3 must be an integer")
  expect_error(boottestonemed(3, .4, 4.3),
	             " * argument 3 must be an integer")
  expect_error(boottestonemed(2, .8, 0.5),
	             " * argument 3 must be an integer")
  expect_error(boottestonemed(2, .8, -100),
	             " * argument 3 must be positive")
})

test_that("output", {
  set.seed(1234)
  samp <- boottestonemed(c(5, 3, 20, 4, 48), .8, 4)

  expect_is(samp, "list")
  expect_is(samp["origtest"], "list")
  expect_is(samp["pvalue"], "list")
  expect_is(samp["teststatall"], "list")
  expect_equal(length(samp$teststatall), 4)
  expect_equal(samp$origtest, 5)
  expect_equal(samp$pvalue, 0)
  expect_equal(samp$teststatall, c(-.2, -.2, -.2, -1.2))

  set.seed(1234)
  samp <- boottestonemed(c(1,-1,6,1), .9, 5)

  expect_equal(samp$pvalue, .4)
})
