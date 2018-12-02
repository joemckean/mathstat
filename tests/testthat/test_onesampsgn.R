# Test for onesampsgn function
#
# Returns:
#    An error message if any tests fail
context("onesampsgn")

# Declare test variables
x <- c(56, 70, 89, 94, 96, 101, 102, 102, 102, 105, 106, 108, 110, 113, 116)
oops_inf <- c(Inf, 1, 2)
oops_nan <- c(1, NaN, 3)
oops_notnumeric <- c(1, 2, "oops")

test_that("edge cases", {
    # Check for x
    expect_error(onesampsgn(Inf),
                 "argument 1 must have length greater than 1")
    expect_error(onesampsgn(oops_inf),
                 "argument 1 cannot include an Inf or -Inf")
    # Check for alt
    expect_error(onesampsgn(x, alt = Inf),
                 "argument 3 cannot be infinite")
    expect_error(onesampsgn(x, alt = -Inf),
                 "argument 3 cannot be infinite")
    # Check for theta0
    expect_error(onesampsgn(x, theta0 = Inf),
                 "argument 4 cannot be infinite")
    expect_error(onesampsgn(x, theta0 = -Inf),
                 "argument 4 cannot be infinite")
    # Check for alpha
    expect_error(onesampsgn(x, alpha = -1),
                 "input argument 'alpha' must be between zero and one")
    expect_error(onesampsgn(x, alpha = 1),
                 "input argument 'alpha' must be between zero and one")
})

test_that("input", {
    # Check for x
    expect_error(onesampsgn(NA),
                 "argument 1 must have length greater than 1")
    expect_error(onesampsgn(oops_nan))
    expect_error(onesampsgn(oops_nan))
    expect_error(onesampsgn(oops_notnumeric),
                 "argument 1 must be a number")
    expect_error(onesampsgn(oops_notnumeric),
                 "argument 1 must be a numeric vector")
    # Check for test
    expect_error(onesampsgn(x, test = "oops"),
                 "argument 2 must be logical")
    expect_error(onesampsgn(x, test = NaN),
                 "argument 2 must be logical")
    # Check for alt
    expect_error(onesampsgn(x, alt = "oops"),
                 "argument 3 must be a number")
    expect_error(onesampsgn(x, alt = NA),
                 "argument 3 must be a number")
    # Check for theta0
    expect_error(onesampsgn(x, theta0 = "oops"),
                 "argument 4 must be a number")
    expect_error(onesampsgn(x, theta0 = NA),
                 "argument 4 must be a number")
    # Check for alpha
    expect_error(onesampsgn(x, alpha = "oops"),
                 "argument 5 must be a number")
    expect_error(onesampsgn(x, alpha = NA),
                 "argument 5 must be a number")
    # Check for maktable
    expect_error(onesampsgn(x, maktable = "oops"),
                 "argument 6 must be logical")
    expect_error(onesampsgn(x, maktable = NaN),
                 "argument 6 must be logical")
    # Check for plotb
    expect_error(onesampsgn(x, plotb = "oops"),
                 "argument 7 must be logical")
    expect_error(onesampsgn(x, plotb = NaN),
                 "argument 7 must be logical")

})

test_that("output", {
    expect_equal(length(names(onesampsgn(x, maktable = FALSE))), 5)
    expect_equal(length(onesampsgn(x, maktable = FALSE)$est), 1)
    expect_equal(length(onesampsgn(x, maktable = FALSE)$est), 1)
    expect_equal(length(onesampsgn(x, maktable = FALSE)$lci), 1)
    expect_equal(length(onesampsgn(x, maktable = FALSE)$uci), 1)
    expect_equal(length(onesampsgn(x, maktable = FALSE)$acconf), 1)
    expect_equal(length(onesampsgn(x, maktable = FALSE)$tau), 1)

    expect_equal(is.numeric(onesampsgn(x, maktable = FALSE)$est), TRUE)
    expect_equal(is.numeric(onesampsgn(x, maktable = FALSE)$est), TRUE)
    expect_equal(is.numeric(onesampsgn(x, maktable = FALSE)$lci), TRUE)
    expect_equal(is.numeric(onesampsgn(x, maktable = FALSE)$uci), TRUE)
    expect_equal(is.numeric(onesampsgn(x, maktable = FALSE)$acconf), TRUE)
    expect_equal(is.numeric(onesampsgn(x, maktable = FALSE)$tau), TRUE)
})


