context("plogF.r")

six_decimal_error <- 1e-07

test_that("input", {
  expect_error(plogF("x"), "argument 1 must be a number")
  expect_error(plogF(Inf), "argument 1 cannot include an Inf or -Inf")
  expect_error(plogF(c(1, 2, Inf)), "argument 1 cannot include an Inf or -Inf")
  expect_error(plogF(NaN), "argument 1 cannot include a NaN")
  expect_error(plogF(c(1, 2, NaN)), "argument 1 cannot include a NaN")
  
})

# Test zero.
test_that("x=0", {
  result <- plogF(0)
  expected <- 0.301172881228421
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})

# Test positive integers.  Steve Yzerman
test_that("x=19", {
  result <- plogF(19)
  expected <- 0.9837861195015
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=14", {
  # Brendan Shanahan
  result <- plogF(14)
  expected <- 0.9559261047282
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=5", {
  # Nicklas Lidstrom
  result <- plogF(5)
  expected <- 0.733440266273
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=24", {
  # Chris Chelios
  result <- plogF(24)
  expected <- 0.994035246701
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})

# Test negative integers.
test_that("x=-19", {
  result <- plogF(-19)
  expected <- 5.60279634336328e-09
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=-14", {
  result <- plogF(-14)
  expected <- 8.3152664478986e-07
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=-5", {
  result <- plogF(-5)
  expected <- 0.0066050239382905
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=-24", {
  result <- plogF(-24)
  expected <- 3.77513454385155e-11
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})


# Test positive fractions.  Random decimal value
test_that("x=3.14159", {
  result <- plogF(3.14159)
  expected <- 0.614003399579
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=0.0000001", {
  # Try very small number
  result <- plogF(1e-07)
  expected <- 0.301172892875
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
# Test negative fractions.  Random decimal value
test_that("x=-10.1234", {
  result <- plogF(-10.1234)
  expected <- 4.0124622e-05
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=-0.0000001", {
  # Try very small number
  result <- plogF(-1e-07)
  expected <- 0.301172869581
  
  expect_that(result, is_a("numeric"))
  expect_that(abs(expected - result) < six_decimal_error, is_true())
})

# Test vector input
test_that("x=(1, 2, 3)", {
  result <- plogF(c(1, 2, 3))
  expected <- c(0.414966745533367, 0.516753801277629, 0.603019935747206)
  
  expect_that(result, is_a("numeric"))
  expect_that(all(abs(expected - result) < six_decimal_error), is_true())
})
