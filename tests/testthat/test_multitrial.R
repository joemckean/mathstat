# Test for multitrial function
#
# Returns:
#    An error message if any tests fail

context("multitrial")

ps <- c(0.3, 0.2, 0.2, 0.2, 0.1)

bs1 <- c(5, 0.3, 0.3, 0.3)

bs2 <- c(1, 0.2, -1, 0.3, 0.3)

test_that("edge cases", {
    expect_error(multitrial(Inf),
                 "argument 1 cannot include an Inf or -Inf")
    expect_error(multitrial(-Inf),
                 "argument 1 cannot include an Inf or -Inf")
    expect_error(multitrial(bs1),
                 "input vector must contain values between zero and one")
    expect_error(multitrial(bs2),
                 "input vector must contain values between zero and one")
})

test_that("input", {
    expect_error(multitrial(Inf),
                 "argument 1 must have length greater than 1")
    expect_error(multitrial(c("a", "b", "c")),
                 "argument 1 must be a number")
    expect_error(multitrial(NA),
                 "argument 1 must be a number")
    expect_error(multitrial(c(.1, .1, .1)),
                 "elements of input value 'p' must sum to one")
    expect_error(multitrial(c(.8, .1, .1, .1)),
                 "elements of input value 'p' must sum to one")
})

test_that("output", {
    expect_equal(length(multitrial(ps)), 1)
    expect_equal(is.numeric(multitrial(ps)), TRUE)
})


