context("wil2powsim.r")

six_decimal_error <- 1e-07

# Test invalid inputs.
test_that("input", {
  expect_error(wil2powsim("n1", 30, 1000, 0.2, 10, c(-3, 3, 1), 0.25), "argument 1 must be a number")
  expect_error(wil2powsim(30, "n2", 1000, 0.2, 10, c(-3, 3, 1), 0.25), "argument 2 must be a number")
  expect_error(wil2powsim(30, 30, "nsims", 0.2, 10, c(-3, 3, 1), 0.25), "argument 3 must be a number")
  expect_error(wil2powsim(30, 30, 1000, "eps", 10, c(-3, 3, 1), 0.25), "argument 4 must be a number")
  expect_error(wil2powsim(30, 30, 1000, 0.2, "vc", c(-3, 3, 1), 0.25), "argument 5 must be a number")
  expect_error(wil2powsim(30, 30, 1000, 0.2, 10, "Delta", 0.25), "argument 6 must be a number")
  expect_error(wil2powsim(30, 30, 1000, 0.2, 10, c(-3, 3, 1), "alpha"), "argument 7 must be a number")
  
  expect_error(wil2powsim(Inf, 30, 1000, 0.2, 10, c(-3, 3, 1), 0.25), "argument 1 cannot include an Inf or -Inf")
  expect_error(wil2powsim(30, Inf, 1000, 0.2, 10, c(-3, 3, 1), 0.25), "argument 2 cannot include an Inf or -Inf")
  expect_error(wil2powsim(30, 30, Inf, 0.2, 10, c(-3, 3, 1), 0.25), "argument 3 cannot include an Inf or -Inf")
  expect_error(wil2powsim(30, 30, 1000, Inf, 10, c(-3, 3, 1), 0.25), "argument 4 cannot include an Inf or -Inf")
  expect_error(wil2powsim(30, 30, 1000, 0.2, Inf, c(-3, 3, 1), 0.25), "argument 5 cannot include an Inf or -Inf")
  expect_error(wil2powsim(30, 30, 1000, 0.2, 10, Inf, 0.25), "argument 6 cannot include an Inf or -Inf")
  expect_error(wil2powsim(30, 30, 1000, 0.2, 10, c(-3, 3, 1), Inf), "argument 7 cannot include an Inf or -Inf")
  
  
  
  expect_error(wil2powsim(NaN, 30, 1000, 0.2, 10, c(-3, 3, 1), 0.25), "argument 1 cannot include a NaN")
  expect_error(wil2powsim(30, NaN, 1000, 0.2, 10, c(-3, 3, 1), 0.25), "argument 2 cannot include a NaN")
  expect_error(wil2powsim(30, 30, NaN, 0.2, 10, c(-3, 3, 1), 0.25), "argument 3 cannot include a NaN")
  expect_error(wil2powsim(30, 30, 1000, NaN, 10, c(-3, 3, 1), 0.25), "argument 4 cannot include a NaN")
  expect_error(wil2powsim(30, 30, 1000, 0.2, NaN, c(-3, 3, 1), 0.25), "argument 5 cannot include a NaN")
  expect_error(wil2powsim(30, 30, 1000, 0.2, 10, NaN, 0.25), "argument 6 cannot include a NaN")
  expect_error(wil2powsim(30, 30, 1000, 0.2, 10, c(-3, 3, 1), NaN), "argument 7 cannot include a NaN")
  
  
  
  expect_error(wil2powsim(-1, 30, 1000, 0.2, 10, c(-3, 3, 1), 0.25), "argument 1 must be positive")
  expect_error(wil2powsim(30, -1, 1000, 0.2, 10, c(-3, 3, 1), 0.25), "argument 2 must be positive")
  expect_error(wil2powsim(30, 30, -1, 0.2, 10, c(-3, 3, 1), 0.25), "argument 3 must be positive")
  expect_error(wil2powsim(30, 30, 1000, -1, 10, c(-3, 3, 1), 0.25), "argument 4 must be positive")
  expect_error(wil2powsim(30, 30, 1000, 0.2, -1, c(-3, 3, 1), 0.25), "argument 5 must be positive")
  expect_error(wil2powsim(30, 30, 1000, 0.2, 10, c(-3, 3, 1), -1), "argument 7 must be positive")
  
  
  expect_error(wil2powsim(0, 30, 1000, 0.2, 10, c(-3, 3, 1), 0.25), "argument 1 must be numeric and non-zero")
  expect_error(wil2powsim(30, 0, 1000, 0.2, 10, c(-3, 3, 1), 0.25), "argument 2 must be numeric and non-zero")
  expect_error(wil2powsim(30, 30, 0, 0.2, 10, c(-3, 3, 1), 0.25), "argument 3 must be numeric and non-zero")
  expect_error(wil2powsim(30, 30, 1000, 0, 10, c(-3, 3, 1), 0.25), "argument 4 must be numeric and non-zero")
  expect_error(wil2powsim(30, 30, 1000, 0.2, 0, c(-3, 3, 1), 0.25), "argument 5 must be numeric and non-zero")
  expect_error(wil2powsim(30, 30, 1000, 0.2, 10, c(-3, 3, 1), 0), "argument 7 must be numeric and non-zero")
  
  # Custom error due to this functions operations. Vector is created with length of argument 2 and
  # arg 6 vector is added to it.  Thus to fit dimensionality arg 2 must be divisible by the length
  # of arg 6. expect_error(wil2powsim(30, 30, 1000, 0.2, 10, c(-3, 3), 0.25), 'argument 6 length
  # must divide argument 2')
  
})

# Test valid input which yield valid results.
test_that("n1=30,n2=30,nsims=1000,eps=0.2,vc=10", {
  result <- wil2powsim(30, 30, 1000, 0.2, 10)
  expect_that(length(result), equals(2))
})
test_that("n1=30,n2=30,nsims=1000,eps=0.2,vc=10,Delta=c(-3,3,1),alpha=0.25", {
  result <- wil2powsim(30, 30, 1000, 0.2, 10, c(-3, 3, 1), 0.25)
  expect_that(length(result), equals(2))
})
