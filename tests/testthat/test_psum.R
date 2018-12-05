# Test for psum function Returns: An error message if any tests
# fail

context("psum")

test_that("edge cases", {
  expect_error(psum(c(0.1, -0.2, 0.2, 0.1, 0.3, 0.1)), "input vector must contain values between zero and one")
})

test_that("input", {
  expect_error(psum(TRUE), "argument 1 must be a numeric vector")
  expect_error(psum(TRUE), "argument 1 must have length greater than 1")
  expect_error(psum(4), "argument 1 must have length greater than 1")
})

test_that("output", {
  expect_equal(psum(c(0.1, 0.1, 0.2, 0.3, 0.2, 0.1)), c(0.1, 0.2, 
    0.4, 0.7, 0.9, 1))
  expect_equal(length(psum(c(0.1, 0.1, 0.2, 0.3, 0.2, 0.1))), 6)
  expect_equal(is.numeric(psum(c(0.1, 0.1, 0.2, 0.3, 0.2, 0.1))), 
    TRUE)
})


