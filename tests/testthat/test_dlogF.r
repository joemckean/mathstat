context("dlogF.r")
#' X is a real number with the following piecewise function:
#'            { 0,   x <= -746
#' dlogF(x) = { pdf, -745 <= x <= 589
#'            { 0,\t 590 <= x <= 709
#'            { NaN, x >= 710

six_decimal_error <- 1e-07

test_that("input", {
  expect_error(dlogF("x"), "argument 1 must be a number")
  expect_error(dlogF(Inf), "argument 1 cannot include an Inf or -Inf")
  expect_error(dlogF(c(1, Inf)), "argument 1 cannot include an Inf or -Inf")
  expect_error(dlogF(NaN), "argument 1 cannot include a NaN")
  expect_error(dlogF(c(1, NaN)), "argument 1 cannot include a NaN")
})

test_that("limits", {
  expect_error(dlogF(710), "all elements in argument 1 must be greater than")
  expect_error(dlogF(-746), "all elements in argument 1 must be greater than")
})

# Test zero.
test_that("x=0", {
  result <- dlogF(0)
  expected <- 0.1164711864619
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})

# Test positive integers.  Steve Yzerman
test_that("x=19", {
  result <- dlogF(19)
  expected <- 0.00324277609606
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=14", {
  # Brendan Shanahan
  result <- dlogF(14)
  expected <- 0.00881477758
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=5", {
  # Nicklas Lidstrom
  result <- dlogF(5)
  expected <- 0.053240200815
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=24", {
  # Chris Chelios
  result <- dlogF(24)
  expected <- 0.0011929506596
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=91", {
  # Sergei Federov
  result <- dlogF(91)
  expected <- 1.807492168e-09
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})

# Test negative integers.
test_that("x=-19", {
  result <- dlogF(-19)
  expected <- 5.6027962491e-09
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=-14", {
  result <- dlogF(-14)
  expected <- 8.315245704824e-07
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=-5", {
  result <- dlogF(-5)
  expected <- 0.006475291832
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=-24", {
  result <- dlogF(-24)
  expected <- 3.775134543423e-11
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=-91", {
  result <- dlogF(-91)
  expected <- 3.01440878506537e-40
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})


# Test positive fractions.  Random decimal value
test_that("x=3.14159", {
  result <- dlogF(3.14159)
  expected <- 0.076537818522
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=0.0000001", {
  # Try very small number
  result <- dlogF(1e-07)
  expected <- 0.116471186461929
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
# Test negative fractions.  Random decimal value
test_that("x=-10.1234", {
  result <- dlogF(-10.1234)
  expected <- 4.0119793e-05
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=-0.0000001", {
  # Try very small number
  result <- dlogF(-1e-07)
  expected <- 0.11647118646192
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})


# Last value before result = 0
test_that("x=-745", {
  result <- dlogF(-745)
  expected <- 4.94065645841247e-324
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})

# Last value before result = NaN
test_that("x=709", {
  result <- dlogF(709)
  expected <- 3.786853123236e-63
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})

test_that("x=(1, 2, 3)", {
  result <- dlogF(c(1, 2, 3))
  expected <- c(0.108987778298344, 0.0941021709526625, 0.0786132284150154)
  
  expect_that(result, is_a("numeric"))
  expect_that(all(abs(expected - result) < six_decimal_error), is_true())
})
