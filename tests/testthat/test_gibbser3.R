# Test for gibbser3 function
#
# Returns:
#    An error message if an tests fail

context("gibbser3")

test_that("edge cases", {
    # Checks for alpha
    expect_error(gibbser3(alpha = -1, beta = 1, nt = 100, m = 3000, n = 6000),
                 "argument 1 must be positive")
    expect_error(gibbser3(alpha = Inf, beta = 1, nt = 100, m = 3000, n = 6000),
                 "argument 1 cannot include an Inf or -Inf")
    expect_error(gibbser3(alpha = -Inf, beta = 1, nt = 100, m = 3000, n = 6000),
                 "argument 1 cannot include an Inf or -Inf")
    # Checks for beta
    expect_error(gibbser3(alpha = 1, beta = -1, nt = 100, m = 3000, n = 6000),
                 "argument 2 must be positive")
    expect_error(gibbser3(alpha = 1, beta = Inf, nt = 100, m = 3000, n = 6000),
                 "argument 2 cannot include an Inf or -Inf")
    expect_error(gibbser3(alpha = 1, beta = -Inf, nt = 100, m = 3000, n = 6000),
                 "argument 2 cannot include an Inf or -Inf")
    # Checks for nt
    expect_error(gibbser3(alpha = 1, beta = 1, nt = -100, m = 3000, n = 6000),
                 "argument 3 must be positive")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = Inf, m = 3000, n = 6000),
                 "argument 3 cannot include an Inf or -Inf")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = -Inf, m = 3000, n = 6000),
                 "argument 3 must be positive")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = -Inf, m = 3000, n = 6000),
                 "argument 3 cannot include an Inf or -Inf")
    # Checks for m
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = -3000, n = 6000),
                 "argument 4 must be positive")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = Inf, n = 6000),
                 "argument 4 cannot include an Inf or -Inf")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = Inf, n = 6000),
                 "argument 4 must be smaller than argument 5")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = -Inf, n = 6000),
                 "argument 4 cannot include an Inf or -Inf")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = -Inf, n = 6000),
                 "argument 4 must be positive")
    # Checks for n
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = 3000, n = -6000),
                 "argument 4 must be smaller than argument 5")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = 3000, n = -6000),
                 "argument 5 must be positive")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = 100, n = Inf),
                 "argument 5 cannot include an Inf or -Inf")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = 100, n = -Inf),
                 "argument 4 must be smaller than argument 5")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = 100, n = -Inf),
                 "argument 5 cannot include an Inf or -Inf")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = 100, n = -Inf),
                 "argument 5 must be positive")
})
test_that("input", {
    # Check for alpha
    expect_error(gibbser3(alpha = NA, beta = 1, nt = 100, m = 100, n = 6000),
                 "argument 1 must be numeric and non-zero")
    expect_error(gibbser3(alpha = NA, beta = 1, nt = 100, m = 100, n = 6000),
                 "argument 1 must be positive")
    expect_error(gibbser3(alpha = NA, beta = 1, nt = 100, m = 100, n = 6000),
                 "argument 1 must be a number")
    expect_error(gibbser3(alpha = "test", beta = 1, nt = 100, m = 100, n = 6000),
                 "argument 1 must be numeric and non-zero")
    expect_error(gibbser3(alpha = "test", beta = 1, nt = 100, m = 100, n = 6000),
                 "argument 1 must be positive")
    expect_error(gibbser3(alpha = "test", beta = 1, nt = 100, m = 100, n = 6000),
                 "argument 1 must be a number")
    # Check for beta
    expect_error(gibbser3(alpha = 1, beta = NA, nt = 100, m = 100, n = 6000),
                 "argument 2 must be numeric and non-zero")
    expect_error(gibbser3(alpha = 1, beta = NA, nt = 100, m = 100, n = 6000),
                 "argument 2 must be positive")
    expect_error(gibbser3(alpha = 1, beta = NA, nt = 100, m = 100, n = 6000),
                 "argument 2 must be a number")
    expect_error(gibbser3(alpha = 1, beta = "test", nt = 100, m = 100, n = 6000),
                 "argument 2 must be numeric and non-zero")
    expect_error(gibbser3(alpha = 1, beta = "test", nt = 100, m = 100, n = 6000),
                 "argument 2 must be positive")
    expect_error(gibbser3(alpha = 1, beta = "test", nt = 100, m = 100, n = 6000),
                 "argument 2 must be a number")
    # Check for nt
    expect_error(gibbser3(alpha = 1, beta = 1, nt = NA, m = 100, n = 6000),
                 "argument 3 must be numeric and non-zero")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = NA, m = 100, n = 6000),
                 "argument 3 must be positive")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = NA, m = 100, n = 6000),
                 "argument 3 must be a number")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = "test", m = 100, n = 6000),
                 "argument 3 must be numeric and non-zero")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = "test", m = 100, n = 6000),
                 "argument 3 must be positive")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = "test", m = 100, n = 6000),
                 "argument 3 must be a number")
    # Check for m
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = "test", n = 6000),
                 "argument 4 must be numeric and non-zero")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = "test", n = 6000),
                 "argument 4 must be positive")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = "test", n = 6000),
                 "argument 4 must be a number")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = "test", n = 6000),
                 "argument 4 must be smaller than argument 5")
    # Check for n
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = 3000, n = "test"),
                 "argument 5 must be numeric and non-zero")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = 3000, n = "test"),
                 "argument 5 must be positive")
    expect_error(gibbser3(alpha = 1, beta = 1, nt = 100, m = 3000, n = "test"),
                 "argument 5 must be a number")
})

test_that("output", {
    expect_equal(length(gibbser3(alpha = 10, beta = 4, nt = 100, m = 30, n = 60)), 4)
    expect_equal(mode(gibbser3(alpha = 10, beta = 4, nt = 100, m = 30, n = 60)$y1), "numeric")
    expect_equal(mode(gibbser3(alpha = 10, beta = 4, nt = 100, m = 30, n = 60)$y2), "numeric")
    expect_equal(mode(gibbser3(alpha = 10, beta = 4, nt = 100, m = 30, n = 60)$x1), "numeric")
    expect_equal(mode(gibbser3(alpha = 10, beta = 4, nt = 100, m = 30, n = 60)$x2), "numeric")

    expect_equal(length(gibbser3(alpha = 10, beta = 4, nt = 100, m = 30, n = 60)$y1),
                 30)
    expect_equal(length(gibbser3(alpha = 10, beta = 4, nt = 100, m = 30, n = 60)$y2),
                 60)
    expect_equal(length(gibbser3(alpha = 10, beta = 4, nt = 100, m = 30, n = 60)$x1),
                 30)
    expect_equal(length(gibbser3(alpha = 10, beta = 4, nt = 100, m = 30, n = 60)$x2),
                 60)
})


