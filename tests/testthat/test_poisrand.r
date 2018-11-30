context("poisrand test")

samp <- "function output"

test_that("input", {
  expect_error(poisrand("x", 7),
	             " * argument 1 must be positive")
  expect_error(poisrand(5, "x"),
	             " * argument 2 must be a number")
  expect_error(poisrand(-1, 4),
	             " * argument 1 must be positive")
  expect_error(poisrand(4.3, 6),
	             " * argument 1 must be an integer")
  expect_error(poisrand(5, -9),
	             " * argument 2 must be positive")
})

test_that("output", {
    set.seed(1234)
    samp <- poisrand(8, 11.5)

    expect_is(samp, "numeric")
    expect_equal(length(samp), 8)
    expect_equal(samp, c(13,16,10,16,14,15,22,8))
})

test_that("limits", {
  expect_error(poisrand(0, 7),
	             " * argument 1 must be numeric and non-zero")
  expect_error(poisrand(7, 0),
	             " * argument 2 must be numeric and non-zero")
  set.seed(1234)
  samp <- poisrand(7, .000000001)
  expect_equal(samp, c(0,0,0,0,0,0,0))

})
