context("qlaplace test")
ep <- 1e-07
samp <- "function return"

test_that("input", {
  expect_error(qlaplace("x"), " * argument 1 must be a numeric vector")
  expect_error(qlaplace(c(0.4, NaN)), " * argument 1 cannot include a NaN")
})

test_that("limits", {
  expect_equal(qlaplace(0), -Inf)
  expect_equal(qlaplace(1), Inf)
  expect_error(qlaplace(-1e-08), " * all elements in argument 1 must be greater than or equal t")
  expect_error(qlaplace(1.0000000001), " * all elements in argument 1 must be greater than or equal t")
})
