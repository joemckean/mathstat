# Test for normaltable function
#
# Returns:
#    An error message if any tests fail

context("normaltable")

test_that("output", {
    expect_equal(mode(normaltable()), "numeric")
    expect_equal(dim(normaltable()), c(36, 10))
    expect_equal(is.matrix(normaltable()), TRUE)
})


