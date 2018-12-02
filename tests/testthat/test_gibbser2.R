# Test for gibbser2 function
#
# Returns:
#    An error message if an tests fail

context("gibbser2")

test_that("edge cases", {
    expect_error(gibbser2(alpha = -1, m = 6000, n = 3000),
                 "argument 1 must be positive")
    expect_error(gibbser2(alpha = 2, m = -1, n = 3000),
                 "argument 2 must be positive")
    expect_error(gibbser2(alpha = 2, m = -1, n = -1),
                 "argument 3 must be positive")
})
test_that("input", {
    expect_error(gibbser2(alpha = 10, m = 6000, n = 3000),
                          "argument 2 must be smaller than argument 3")
    expect_error(gibbser2(alpha = 2, m = 0, n = 100),
                 "argument 2 must be numeric and non-zero")
    expect_error(gibbser2(alpha = "test", m = 3000, n = 6000),
                 "argument 1 must be numeric and non-zero")
    expect_error(gibbser2(alpha = 2, m = "test", n = 6000),
                 "argument 2 must be numeric and non-zero")
    expect_error(gibbser2(alpha = 2, m = 3000, n = "test"),
                 "argument 3 must be numeric and non-zero")
})

test_that("output", {
    expect_equal(length(gibbser2(alpha = 10, m = 30, n = 60)), 4)
    expect_equal(mode(gibbser2(alpha = 10, m = 30, n = 60)$y1), "numeric")
    expect_equal(mode(gibbser2(alpha = 10, m = 30, n = 60)$y2), "numeric")
    expect_equal(mode(gibbser2(alpha = 10, m = 30, n = 60)$x1), "numeric")
    expect_equal(mode(gibbser2(alpha = 10, m = 30, n = 60)$x2), "numeric")

    expect_equal(length(gibbser2(alpha = 10, m = 30, n = 60)$y1),
                 30)
    expect_equal(length(gibbser2(alpha = 10, m = 30, n = 60)$y2),
                 60)
    expect_equal(length(gibbser2(alpha = 10, m = 30, n = 60)$x1),
                 30)
    expect_equal(length(gibbser2(alpha = 10, m = 30, n = 60)$x2),
                 60)
})


