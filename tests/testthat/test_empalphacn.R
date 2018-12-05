context("empalphacn test")

ep <- 1e-07

test_that("input", {
  expect_error(empalphacn(NaN), " * argument 1 cannot include a NaN")
  expect_error(empalphacn(Inf), " * argument 1 cannot include an Inf or -Inf")
  expect_error(empalphacn("a"), " * argument 1 must be an integer")
  
})

test_that("output", {
  set.seed(1234)
  samp <- empalphacn(10)
  expect_true(samp$empiricalalpha >= 0.1 - ep && samp$empiricalalpha <= 0.1 + ep)
  expect_true(samp$error >= 0.1859419 - ep && samp$error <= 0.1859419 + ep)
})

test_that("limits", {
  set.seed(12345)
  samp <- empalphacn(1)
  expect_equal(samp$empiricalalpha, 0)
  expect_equal(samp$error, 0)
  expect_error(empalphacn(0.5), " * argument 1 must be an integer")
  expect_error(empalphacn(0), " * argument 1 must be numeric and non-zero")
  expect_error(empalphacn(-1), " * argument 1 must be positive")
})
