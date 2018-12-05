context("tpowerg.r")
library(visualTest)  #for comparing graph output
png("../test_img/tpowerg_test.png")
tpowerg(50, 10, 25)
dev.off()

six_decimal_error <- 1e-07

# Test invalid inputs.
test_that("input", {
  expect_error(tpowerg("mu0", 10, 25, 0.04, 0.2), "argument 1 must be a number")
  expect_error(tpowerg(50, "sig", 25, 0.04, 0.2), "argument 2 must be a number")
  expect_error(tpowerg(50, 10, "n", 0.04, 0.2), "argument 3 must be a number")
  expect_error(tpowerg(50, 10, 25, "aplha", 0.2), "argument 4 must be a number")
  expect_error(tpowerg(50, 10, 25, 0.04, "byv"), "argument 5 must be a number")
  
  expect_error(tpowerg(Inf, 10, 25, 0.04, 0.2), "argument 1 cannot include an Inf or -Inf")
  expect_error(tpowerg(50, Inf, 25, 0.04, 0.2), "argument 2 cannot include an Inf or -Inf")
  expect_error(tpowerg(50, 10, Inf, 0.04, 0.2), "argument 3 cannot include an Inf or -Inf")
  expect_error(tpowerg(50, 10, 25, Inf, 0.2), "argument 4 cannot include an Inf or -Inf")
  expect_error(tpowerg(50, 10, 25, 0.04, Inf), "argument 5 cannot include an Inf or -Inf")
  
  expect_error(tpowerg(NaN, 10, 25, 0.04, 0.2), "argument 1 cannot include a NaN")
  expect_error(tpowerg(50, NaN, 25, 0.04, 0.2), "argument 2 cannot include a NaN")
  expect_error(tpowerg(50, 10, NaN, 0.04, 0.2), "argument 3 cannot include a NaN")
  expect_error(tpowerg(50, 10, 25, NaN, 0.2), "argument 4 cannot include a NaN")
  expect_error(tpowerg(50, 10, 25, 0.04, NaN), "argument 5 cannot include a NaN")
  
  expect_error(tpowerg(-1, 10, 25, 0.04, 0.2), "argument 1 must be positive")
  expect_error(tpowerg(50, -10, 25, 0.04, 0.2), "argument 2 must be positive")
  expect_error(tpowerg(50, 10, -25, 0.04, 0.2), "argument 3 must be positive")
  expect_error(tpowerg(50, 10, 25, -0.04, 0.2), "argument 4 must be positive")
  expect_error(tpowerg(50, 10, 25, 0.04, -0.2), "argument 5 must be positive")
  
  expect_error(tpowerg(0, 10, 25, 0.04, 0.2), "argument 1 must be numeric and non-zero")
  expect_error(tpowerg(50, 0, 25, 0.04, 0.2), "argument 2 must be numeric and non-zero")
  expect_error(tpowerg(50, 10, 0, 0.04, 0.2), "argument 3 must be numeric and non-zero")
  expect_error(tpowerg(50, 10, 25, 0, 0.2), "argument 4 must be numeric and non-zero")
  expect_error(tpowerg(50, 10, 25, 0.04, 0), "argument 5 must be numeric and non-zero")
})

# Test valid input which yield valid results.

test_that("mu=50, sig=10, n=25", {
  finger <- getFingerprint(file = "../test_img/tpowerg_key.png")
  expect_true(isSimilar(file = "../test_img/tpowerg_test.png", finger, threshold = 25))
})
