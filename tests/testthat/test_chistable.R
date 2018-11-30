# Test for chistable function
#
# Returns:
#    An error message if any tests fail

context("chistable")

test_that("output", {
    expect_equal(mode(chistable()), "numeric")
    expect_equal(dim(chistable()), c(30, 9))
    expect_equal(is.matrix(chistable()), TRUE)
})


