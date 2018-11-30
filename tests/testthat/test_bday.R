# Test spike for bday
#
# Returns:
#    An error message if an tests fail
context("bday")

test_that("edge cases", {
    expect_error(bday(366),
                 "'n' cannot be larger than 365")
    expect_error(bday(-1),
                 "argument 1 must be positive")
})
test_that("input", {
    expect_error(bday(), "argument 1 must be numeric and non-zero")
    expect_error(bday("f"), "argument 1 must be numeric and non-zero")
})

test_that("output",  {
    expect_equal(bday(2), 0.002739726)
    expect_equal(is.numeric(bday(2)), TRUE)
    expect_equal(is.vector(bday(2)), TRUE)
})
