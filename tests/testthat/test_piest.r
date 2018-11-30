context("piest.r")

six_decimal_error <- 0.0000001

# Test invalid inputs.
test_that("input", {
	expect_error(piest("x"), "argument 1 must be a number")
	expect_error(piest(c(1,2,3)), "argument 1 cannot have length greater than 1")
	expect_error(piest(Inf), "argument 1 cannot include an Inf or -Inf")
	expect_error(piest(NaN), "argument 1 cannot include a NaN")
	expect_error(piest(-1), "argument 1 must be positive")
	expect_error(piest(0), "argument 1 must be numeric and non-zero")
})

# Test valid inputs expecting valid results.
test_that("n=3", {
	result <- piest(3)

	expect_that(result, is_a("list"))
	expect_that(length(result), equals(2))
	expect_that(result$estimate >= 0, is_true())
	expect_that(result$standard_error >= 0, is_true())
})

test_that("n=5000", {
	result <- piest(5000)
	expect_that(result, is_a("list"))
	expect_that(length(result), equals(2))
	expect_that(abs(3.14-result$estimate) < 0.1, is_true())
	expect_that(result$standard_error < 0.1, is_true())
})


# Test valid input which results in invalid result.
test_that("n=5000000000", {
	expect_that(piest(5000000000), throws_error("cannot allocate vector"))
})
