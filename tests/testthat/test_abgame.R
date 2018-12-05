# Test spike for abgame function Returns: An error message and a
# score of 1 for each assert that fails
context("abgame")

test_that("output", {
  expect_equal(length(abgame()), 2)
  expect_equal(is.vector(abgame()), TRUE)
  
})



