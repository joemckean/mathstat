# Test for getcis function Returns: An error message if any tests fail
context("getcis")

n <- 10
m <- 50

set.seed(111)

mat <- matrix(rnorm(m * n), ncol = n)

NAcheck <- rep(NA, m)
dummymat <- cbind(mat, NAcheck)

Infcheck <- rep(Inf, m)
dummymat2 <- cbind(mat, Infcheck)

# Insert test_that for edge cases

test_that("edge cases", {
  expect_error(getcis(mat, 2))
  expect_error(getcis(mat, -2))
})

test_that("input", {
  expect_equal(dim(getcis(mat)), c(50, 2))
  expect_error(getcis(c(2, 3, 3), cc = 0.975), "argument 1 must be a matrix")
})

test_that("output", {
  expect_equal(getcis(mat)[1, ], c(-0.58602443, 0.0778093))
  expect_equal(length(getcis(mat, cc = 0.95)[1, ]), 2)
})


