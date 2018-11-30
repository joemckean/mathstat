# Test for ttable function
#
# Returns:
#    An error message if any tests fail

context("ttable")

test_that("output", {
    expect_equal(mode(ttable()), "numeric")
    expect_equal(dim(ttable()), c(31, 8))
    expect_equal(is.matrix(ttable()), TRUE)
})


