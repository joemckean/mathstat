# Test for mlelogistic function
#
# Returns:
#    An error message if an tests fail
context("mlelogistic")

x <- c(200, 230, 254, 301, 354, 361, 372, 405, 413, 415, 418, 419, 423, 445)

test_that("edge cases", {
    expect_error(mlelogistic(Inf),
                 "argument 1 cannot include an Inf or -Inf")
    expect_error(mlelogistic(-Inf),
                 "argument 1 cannot include an Inf or -Inf")
    expect_error(mlelogistic(x, eps = 1.1),
                 "input argument 'eps' must be between zero and one")
    expect_error(mlelogistic(x, eps = -1),
                 "argument 2 must be positive")
})

test_that("input", {
    expect_error(mlelogistic("test"),
                 "argument 1 must be a numeric vector")
    expect_error(mlelogistic(NA),
                 "argument 1 must be a numeric vector")
    expect_error(mlelogistic(x, eps = "test"),
                 "argument 2 must be a numeric vector")
    expect_error(mlelogistic(x, eps = "test"),
                 "argument 2 must be positive")
    expect_error(mlelogistic(x, eps = NA),
                 "argument 2 must be a numeric vector")
    expect_error(mlelogistic(x, eps = NA),
                 "argument 2 must be positive")
})

test_that("output", {
    expect_equal(round(mlelogistic(x)$theta1), 388)
    expect_equal(mlelogistic(x)$check, 6.233679e-08)
    expect_equal(mlelogistic(x)$realnumstps, 6)
    expect_equal(is.numeric(mlelogistic(x)$theta1), TRUE)
    expect_equal(length(mlelogistic(x)), 3)
    expect_equal(length(mlelogistic(x)$theta1), 1)
    expect_equal(length(mlelogistic(x)$check), 1)
    expect_equal(length(mlelogistic(x)$realnumstps), 1)
})


