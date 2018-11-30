context("arg_check test")
# for is_positive function
test_that("is_positive works", {
  samp <- is_positive(4, 1)
  expect_equal(samp, NULL)

  samp <- is_positive(-7, 1)
  expect_equal(samp, "argument 1 must be positive")

  samp <- is_positive(0, 1)
  expect_equal(samp, NULL)

  samp <- is_positive("r is fun", 1)
  expect_equal(samp, "argument 1 must be positive")

  expect_warning(is_positive(c(5, 4, 3.4), 1), "the condition has length > 1 and only the first element will be used")
  expect_equal(is_positive(-3, 1, "hey"), "hey")
})

# for is_negative() function
test_that("is_negative works", {

  samp <- is_negative(-7, 1)
  expect_equal(samp, NULL)

  samp <- is_negative(4, 1)
  expect_equal(samp, "argument 1 must be negative")

  samp <- is_negative(0, 1)
  expect_equal(samp, NULL)

  samp <- is_negative("r is fun", 1)
  expect_equal(samp, "argument 1 must be negative")

  expect_warning(
    is_negative(c(5, 4, 3.4), 1),
    "the condition has length > 1 and only the first element will be used"
  )
})

# for is_nonzero() function
test_that("is_nonzero works", {

  samp <- is_nonzero(0, 1)
  expect_equal(samp, "argument 1 must be numeric and non-zero")

  samp <- is_nonzero(4, 1)
  expect_equal(samp, NULL)

  samp <- is_nonzero(-7, 1)
  expect_equal(samp, NULL)

  samp <- is_nonzero("r is fun", 1)
  expect_equal(samp, "argument 1 must be numeric and non-zero")

  expect_warning(
    is_nonzero(c(5, 4, 3.4), 1),
    "the condition has length > 1 and only the first element will be used"
  )

})

# for is_integer() function
test_that("is_integer works", {

  samp <- is_integer(0, 1)
  expect_equal(samp, NULL)

  samp <- is_integer(92, 1)
  expect_equal(samp, NULL)

  samp <- is_integer(4 / 2, 1)
  expect_equal(samp, NULL)

  samp <- is_integer(-5, 1)
  expect_equal(samp, NULL)

  samp <- is_integer(5.293, 1)
  expect_equal(samp, "argument 1 must be an integer")

  samp <- is_integer(3 / 2, 1)
  gives_warning(samp, "argument 1 must be an integer")

  samp <- is_integer("r is fun", 1)
  expect_equal(samp, "argument 1 must be an integer")

})

# for is_vector() function
test_that("is_numvector works", {
  samp <- is_numvector(c(3, -2, 1), 1)
  expect_equal(samp, NULL)

  samp <- is_numvector("hey", 1)
  expect_equal(samp, "argument 1 must be a numeric vector")

})

# for is_numeric() function
test_that("is_numeric works", {
  samp <- is_numeric(3, 1)
  expect_equal(samp, NULL)

  samp <- is_numeric(-3.44, 1)
  expect_equal(samp, NULL)

  samp <- is_numeric("r is fun", 1)
  expect_equal(samp, "argument 1 must be a number")

  samp <- is_numeric("six", 1)
  gives_warning(samp, "argument 1 must be a number")

  expect_equal(is_numeric("x", 2, "hey"), "hey")

})

test_that("is_manyelement works", {
	expect_equal(is_manyelement(1, 1),
	             "argument 1 must have length greater than 1")
	expect_equal(is_manyelement(c(1,2,3), 1), NULL)
	expect_equal(is_manyelement("xst", 1),
               "argument 1 must have length greater than 1")

})

#for is_smaller() function
test_that("is_smaller works", {
  samp <- is_smaller(1, 2, 1, 2)
  expect_equal(samp, NULL)

  samp <- is_smaller(1, 1.1, 1, 2)
  expect_equal(samp, NULL)

  samp <- is_smaller(2, 1, 1, 2)
  expect_equal(samp, "argument 1 must be smaller than argument 2")

  samp <- is_smaller(2, -3, 1, 2)
  expect_equal(samp, "argument 1 must be smaller than argument 2")

  samp <- is_smaller(2, 1.9, 1, 2)
  expect_equal(samp, "argument 1 must be smaller than argument 2")

})

test_that("is_inrange works",{
  # works in middle
  samp <- is_inrange(.5, 1, 0, 1)
  expect_equal(samp, NULL)
  # works when x = min
  samp <- is_inrange(1, 1, 1, 100)
  expect_equal(samp, NULL)
  # works when x = max
  samp <- is_inrange(1, 1, 0, 1)
  expect_equal(samp, NULL)
  # rejects when x < min
  samp <- is_inrange(-.00000000001, 1, 0, 1)
  expect_equal(samp, "argument 1 must be greater than or equal to 0 and less than or equal to 1")
  # rejects when x > max
  samp <- is_inrange(9.0000000000001, 1, 1, 9)
  expect_equal(samp, "argument 1 must be greater than or equal to 1 and less than or equal to 9")
  # rejects when x is of length > 1
  samp <- is_inrange(c(1,2,3,4), 1, 0, 5)
  expect_equal(samp, "argument 1 cannot have a length greater than 1")
  # works when range is infinity
  samp <- is_inrange(1920, 1, -Inf, Inf)
  expect_equal(samp, NULL)
  # rejects when x is inf and min and max are not
  samp <- is_inrange(Inf, 1, 0, 3)
  expect_equal(samp,
		           "argument 1 must be greater than or equal to 0 and less than or equal to 3")
  expect_equal(is_inrange(0, 1, 1, 3, "hey"), "hey")
  })

test_that("is_xrange works",{
  # works in middle
  samp <- is_xrange(.5, 1, 0, 1)
  expect_equal(samp, NULL)
  # works when x = min
  samp <- is_xrange(1.0000001, 1, 1, 100)
  expect_equal(samp, NULL)
  # works when x = max
  samp <- is_xrange(0.99999999, 1, 0, 1)
  expect_equal(samp, NULL)
  # rejects when x < min
  samp <- is_xrange(0, 1, 0, 1)
  expect_equal(samp, "argument 1 must be greater than 0 and less than 1")
  # rejects when x > max
  samp <- is_xrange(9, 1, 1, 9)
  expect_equal(samp, "argument 1 must be greater than 1 and less than 9")
  # rejects when x is of length > 1
  samp <- is_xrange(c(1,2,3,4), 1, 0, 5)
  expect_equal(samp, "argument 1 cannot have a length greater than 1")
  # works when range is infinity
  samp <- is_xrange(1920, 1, -Inf, Inf)
  expect_equal(samp, NULL)
  # rejects when x is inf and min and max are not
  samp <- is_xrange(Inf, 1, 0, 3)
  expect_equal(samp, "argument 1 must be greater than 0 and less than 3")
  expect_equal(is_xrange(7, 1, 2, 3, "hey"), "hey")
})

test_that("is_noninf works", {
    expect_equal(is_noninf(5, 1), NULL)
    expect_equal(is_noninf(-5, 1), NULL)
    expect_equal(is_noninf(Inf, 1), "argument 1 cannot be infinite")
    expect_equal(is_noninf(-Inf, 1), "argument 1 cannot be infinite")
    expect_equal(is_noninf(1/0, 1), "argument 1 cannot be infinite")
    expect_equal(is_noninf(NaN, 1), NULL)
    expect_equal(is_noninf("x", 1), NULL)
})

test_that("has_noinf works", {
	expect_equal(has_noinf(3, 1), NULL)
	expect_equal(has_noinf(c(3, 4, 3), 1), NULL)
	expect_equal(has_noinf(Inf, 1), "argument 1 cannot include an Inf or -Inf")
	expect_equal(has_noinf(-Inf, 1), "argument 1 cannot include an Inf or -Inf")
	expect_equal(has_noinf(c(1, 3, 2, Inf, 3), 1),
		           "argument 1 cannot include an Inf or -Inf")
	expect_equal(has_noinf("x", 1), NULL)
})

test_that("has_nonan works", {
	expect_equal(has_nonan(3, 1), NULL)
	expect_equal(has_nonan(c(3, 4, 3), 1), NULL)
#	expect_equal(has_nonan(NaN, 1))
#	expect_equal(has_nonan(c(1, 3, 2, NaN, 3), 1))
	expect_equal(has_nonan("x", 1), NULL)
})

test_that("is_vecinrange works", {
  expect_equal(is_vecinrange(c(1,2,3), 1, 1, 3), NULL)
  expect_equal(is_vecinrange(2, 1, 1, 3), NULL)
  expect_equal(is_vecinrange(c(1.5, .999999999, 1, 2), 1, 1, 2), "all elements in argument 1 must be greater than or equal to 1 and less than or equal to 2")
  expect_equal(is_vecinrange(c(1,2,2.00000000001,0), 1, 0, 2), "all elements in argument 1 must be greater than or equal to 0 and less than or equal to 2")
  expect_equal(is_vecinrange(c(1,2,3), 1, -Inf, Inf), NULL)
  expect_equal(is_vecinrange(c(Inf, 2,3), 1, -6, 9), "all elements in argument 1 must be greater than or equal to -6 and less than or equal to 9")
  expect_equal(is_vecinrange(c(NaN, 1, 2), 1, 0, 2), NULL)
  expect_equal(is_vecinrange(c(2, "c", 8),1, 1, 9), "all elements in argument 1 must be greater than or equal to 1 and less than or equal to 9")
})

test_that("is_vecxrange works", {
    expect_equal(is_vecxrange(c(1,2,2.5), 1, 1, 3), "all elements in argument 1 must be greater than 1 and less than 3")
    expect_equal(is_vecxrange(2, 1, 1, 3), NULL)
    expect_equal(is_vecxrange(c(1.5, 1.0000001, 2), 1, 1, 3), NULL)
    expect_equal(is_vecxrange(c(1,.5,1.5,1.9999999), 1, 0, 2), NULL)
    expect_equal(is_vecxrange(c(1,2,3), 1, -Inf, Inf), NULL)
    expect_equal(is_vecxrange(c(Inf, 2,3), 1, -6, 9), "all elements in argument 1 must be greater than -6 and less than 9")
    expect_equal(is_vecxrange(c(NaN, 1, 2), 1, 0, 3), NULL)
    expect_equal(is_vecxrange(c(2, "c", 8),1, 1, 9), "all elements in argument 1 must be greater than 1 and less than 9")
})

test_that("has_elements works", {
	expect_equal(has_elements(c(1,2), 1, 2), NULL)
	expect_equal(has_elements(c(1), 1, 2), "argument 1 must have 2 elements")
	expect_equal(has_elements("as", 1, 2), "argument 1 must have 2 elements")
})

test_that("is_matrix works", {
	expect_equal(is_matrix("x", 1), "argument 1 must be a matrix")
	expect_equal(is_matrix(c(1,2,3), 1), "argument 1 must be a matrix")
	expect_equal(is_matrix(4, 1), "argument 1 must be a matrix")
	expect_equal(is_matrix(matrix(c(1,2,3,4), nrow = 2)), NULL)
})

test_that("is_posmatrix2 works", {
	expect_equal(is_posmatrix2(matrix(c(1,2,3,4), nrow = 2), 1), NULL)
	expect_equal(is_posmatrix2(matrix(c(0,0,0,0), nrow = 2), 1), NULL)
	expect_equal(is_posmatrix2(matrix(c(1,2,-3,4), nrow = 2), 1), "argument 1 must have all positive entries")
	expect_equal(is_posmatrix2(matrix(c(-1,-2,-3,-4), nrow = 2), 1), "argument 1 must have all positive entries")
})

test_that("is_posdetmat2 works", {
	expect_equal(is_posdetmat2("x", 1), "argument 1 must be a matrix")
	expect_equal(is_posdetmat2(c(1,2,3), 1), "argument 1 must be a matrix")
	expect_equal(is_posdetmat2(4, 1), "argument 1 must be a matrix")
	expect_equal(is_posdetmat2(matrix(c(1,2,3,6), nrow = 2), 1), "the determinate of argument 1 must be positive")
	expect_equal(is_posdetmat2(matrix(c(2,2,3,4), nrow = 2), 1), NULL)
})
