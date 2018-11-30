# Test spike for rcn function
#
# Returns:
#    An error message if an tests fail
context("RCN Limits")

set.seed(1)

test_that("validating rcn input", {
    #expect_equal(rcn(n = 0, eps = 0.20, sigma_c = 2), numeric(0))
    #expect_warning(rcn(n = 0, eps = 0.20, sigma_c = 2))
    expect_error(rcn(n = -1, eps = 0.20, sigma_c = 2))
    expect_error(rcn(n = 10000e5, eps = 0.20, sigma_c = 2))
})

test_that("validating rcn output", {
    expect_equal(rcn(n = 12, eps = 0.20, sigma_c = 4), c(0.48742905, 0.73832471,
                                                       0.57578135, -1.22155355,
                                                       1.51178117,  1.55937295,
                                                       -2.48496232, -2.21469989,
                                                       1.12493092, -0.04493361,
                                                       -0.01619026,  0.94383621))
    expect_equal(is.numeric(rcn(n = 14, eps = 0.15, sigma_c = 2)), TRUE)
    expect_equal(length(rcn(n = 25, eps = 0.10, sigma_c = 2)), 25)
})


