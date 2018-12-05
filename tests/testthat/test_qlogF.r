context("qlogF.r")
# X is a real number with the following piecewise function: { NaN, x < 0 { -Inf, x = 0 qlogF(x)
# = { n, 0 < x < 1 { Inf, x = 1 { NaN, x > 1

six_decimal_error <- 1e-07

test_that("input", {
  expect_error(qlogF("x"), "argument 1 must be a number")
  expect_error(qlogF(Inf), "argument 1 cannot include an Inf or -Inf")
  expect_error(qlogF(c(0.1, Inf)), "argument 1 cannot include an Inf or -Inf")
  expect_error(qlogF(NaN), "argument 1 cannot include a NaN")
  expect_error(qlogF(c(0.1, NaN)), "argument 1 cannot include a NaN")
})

test_that("limits", {
  expect_error(qlogF(-1), "all elements in argument 1 must be greater")
  expect_error(qlogF(0), "all elements in argument 1 must be greater")
  expect_error(qlogF(1), "all elements in argument 1 must be greater")
  expect_error(qlogF(2), "all elements in argument 1 must be greater")
  expect_error(qlogF(c(0.1, 0.2, 2)), "all elements in argument 1 must be greater than")
})

# Test values within acceptable range.
test_that("x=0.5", {
  result <- qlogF(0.5)
  expected <- 1.824549292051
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=0.1", {
  result <- qlogF(0.1)
  expected <- -1.9754292901
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=0.9", {
  result <- qlogF(0.9)
  expected <- 9.90347755248
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=0.0000000001", {
  # Very small number
  result <- qlogF(1e-10)
  expected <- -23.02585092964
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=0.9999999999", {
  # Very close to 1
  result <- qlogF(0.9999999999)
  expected <- 113.519816323566
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=(0.1, 0.2, 0.3)", {
  result <- qlogF(c(0.1, 0.2, 0.3))
  expected <- c(-1.97542929010773, -0.890741017188553, -0.0100701690562664)
  
  expect_that(result, is_a("numeric"))
  expect_that(all(abs(expected - result) < six_decimal_error), is_true())
})
