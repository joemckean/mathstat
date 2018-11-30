context("plogF.r")

six_decimal_error <- 0.0000001

test_that("input", {
	expect_error(plogF("x"), "argument 1 must be a number")
	expect_error(plogF(Inf), "argument 1 cannot include an Inf or -Inf")
	expect_error(plogF(c(1, 2, Inf)), "argument 1 cannot include an Inf or -Inf")
	expect_error(plogF(NaN), "argument 1 cannot include a NaN")
	expect_error(plogF(c(1, 2, NaN)), "argument 1 cannot include a NaN")

})

# Test zero.
test_that("x=0", {
	result <- plogF(0)
	expected <- 0.30117288122842075

	expect_that(result, is_a("numeric"))
	expect_that(abs(expected - result) < six_decimal_error, is_true())
})

# Test positive integers.
test_that("x=19", { # Steve Yzerman
	result <- plogF(19)
	expected <- 0.9837861195015

	expect_that(result, is_a("numeric"))
	expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=14", { # Brendan Shanahan
	result <- plogF(14)
	expected <- 0.9559261047282

	expect_that(result, is_a("numeric"))
	expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=5", { # Nicklas Lidstrom
	result <- plogF(5)
	expected <- 0.733440266273

	expect_that(result, is_a("numeric"))
	expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=24", { # Chris Chelios
	result <- plogF(24)
	expected <- 0.994035246701

	expect_that(result, is_a("numeric"))
	expect_that(abs(expected - result) < six_decimal_error, is_true())
})

# Test negative integers.
test_that("x=-19", {
	result <- plogF(-19)
	expected <- 5.60279634336328e-9

	expect_that(result, is_a("numeric"))
	expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=-14", {
	result <- plogF(-14)
	expected <- 8.31526644789860e-7

	expect_that(result, is_a("numeric"))
	expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=-5", {
	result <- plogF(-5)
	expected <- 0.0066050239382905

	expect_that(result, is_a("numeric"))
	expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=-24", {
	result <- plogF(-24)
	expected <- 3.775134543851548e-11

	expect_that(result, is_a("numeric"))
	expect_that(abs(expected - result) < six_decimal_error, is_true())
})


# Test positive fractions.
test_that("x=3.14159", { # Random decimal value
	result <- plogF(3.14159)
	expected <- 0.614003399579

	expect_that(result, is_a("numeric"))
	expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=0.0000001",{ # Try very small number
	result <- plogF(0.0000001)
	expected <- 0.301172892875

	expect_that(result, is_a("numeric"))
	expect_that(abs(expected - result) < six_decimal_error, is_true())
})
# Test negative fractions.
test_that("x=-10.1234", { # Random decimal value
	result <- plogF(-10.1234)
	expected <- 0.000040124622

	expect_that(result, is_a("numeric"))
	expect_that(abs(expected - result) < six_decimal_error, is_true())
})
test_that("x=-0.0000001", { # Try very small number
	result <- plogF(-0.0000001)
	expected <- 0.301172869581

	expect_that(result, is_a("numeric"))
	expect_that(abs(expected - result) < six_decimal_error, is_true())
})

# Test vector input
test_that("x=(1, 2, 3)", {
	result <- plogF(c(1, 2, 3))
	expected <- c(0.41496674553336699,
				  0.51675380127762893,
				  0.60301993574720614)

	expect_that(result, is_a("numeric"))
	expect_that(all(abs(expected - result) < six_decimal_error), is_true())
})
