# Test for fp2 function
#
# Returns:
#    An error message if any tests fail

context("fp2")

test_that("output", {
    expect_equal(mode(fp1()), "numeric")
    expect_equal(dim(fp1()), c(35, 10))
    expect_equal(is.matrix(fp1()), TRUE)
})


