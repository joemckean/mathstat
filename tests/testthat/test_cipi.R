# Test for cipi function
#
# Returns:
#    An error message if an tests fail

context("cipi")

# Load data set
load(url('http://www.stat.wmich.edu/mckean/hmchomepage/Data/men1500m.rda'))

# Declare variables
year <- men1500m$year
time <- men1500m$time

test_that("edge cases", {
    expect_error(cipi(fit = lm(time ~ year), hmat = c(1)),
                 "length of hmat must match number of coefficients of linear model")
    expect_error(cipi(fit = lm(time ~ year), hmat = c(1, 2020, 2021)),
                 "length of hmat must match number of coefficients of linear model")
    expect_error(cipi(fit = lm(time ~ year), hmat = c(1, 2020), alpha = 2),
                 "input argument 'alpha' must be between zero and one")
    expect_error(cipi(fit = lm(time ~ year), hmat = c(1, 2020), alpha = -2),
                 "input argument 'alpha' must be between zero and one")
})

test_that("input", {
    expect_error(cipi(fit = c(1, 2, 3), hmat = c(1, 2020)),
                 "argument 1 must be a linear model")
    expect_error(cipi(fit = matrix(c(1, 2, 3, 4)), hmat = c(1, 2020)),
                 "argument 1 must be a linear model")
    expect_error(cipi(fit = data.frame(c(1, 2, 3, 4)), hmat = c(1, 2020)),
                 "argument 1 must be a linear model")
    expect_error(cipi(fit = lm(time ~ year), hmat = "test"),
                 "argument 2 must be a number")
    expect_error(cipi(fit = lm(time ~ year), hmat = NA),
                 "argument 2 must be a number")
    expect_error(cipi(fit = lm(time ~ year), hmat = c(1, 2020), alpha = "test"),
                 "argument 3 must be a number")
    expect_error(cipi(fit = lm(time ~ year), hmat = c(1, 2020), alpha = NA),
                 "argument 3 must be a number")
    expect_error(cipi(fit = lm(time ~ year), hmat = c(1, 2020), alpha = c(1, 2)))
})

test_that("output", {
    expect_equal(colnames(cipi(fit = lm(time ~ year), hmat = c(1, 2020))),
                 c("Pred", "SECI", "LCI",  "UCI", "Pred", "SEPI", "LPI", "UPI"))
    expect_equal(is.numeric(cipi(fit = lm(time ~ year), hmat = c(1, 2020))[,1]), TRUE)
    expect_equal(is.numeric(cipi(fit = lm(time ~ year), hmat = c(1, 2020))[,2]), TRUE)
    expect_equal(is.numeric(cipi(fit = lm(time ~ year), hmat = c(1, 2020))[,3]), TRUE)
    expect_equal(is.numeric(cipi(fit = lm(time ~ year), hmat = c(1, 2020))[,4]), TRUE)
    expect_equal(is.numeric(cipi(fit = lm(time ~ year), hmat = c(1, 2020))[,5]), TRUE)
    expect_equal(is.numeric(cipi(fit = lm(time ~ year), hmat = c(1, 2020))[,6]), TRUE)
    expect_equal(is.numeric(cipi(fit = lm(time ~ year), hmat = c(1, 2020))[,7]), TRUE)
    expect_equal(is.numeric(cipi(fit = lm(time ~ year), hmat = c(1, 2020))[,8]), TRUE)
})


