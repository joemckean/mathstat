context("qqplotc4s2.r")
library(visualTest) #for comparing graph output
png("../test_img/qqplotc4s2_test.png")
qqplotc4s2(1:1000)
dev.off()

six_decimal_error <- 0.0000001

# Test invalid inputs.
test_that("input", {
	expect_error(qqplotc4s2("vec"), "argument 1 must be a number")
	expect_error(qqplotc4s2(Inf), "argument 1 cannot include an Inf or -Inf")
	expect_error(qqplotc4s2(NaN), "argument 1 cannot include a NaN")
})

# Test valid input which yield valid results.

test_that("vec = 1:1000", {
	finger <- getFingerprint(file = "../test_img/qqplotc4s2_key.png")
  expect_true(isSimilar(file = "../test_img/qqplotc4s2_test.png", finger, threshold = 8))
})
