# Test for ftableb function Returns: An error message if any tests fail

context("ftableb")

test_that("output", {
  expect_equal(mode(ftableb()), "numeric")
  expect_equal(dim(ftableb()), c(48, 18))
  expect_equal(is.matrix(ftableb()), TRUE)
})


