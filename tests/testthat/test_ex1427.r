context("ex1427 test")
samp <- ex1427()

test_that("output", {
  expect_is(samp, "numeric")
  expect_equal(samp%%1, 0)
  expect_true(samp == 1 || samp == 0)
  set.seed(1234)
  expect_equal(ex1427(), 0)
  set.seed(12345)
  expect_equal(ex1427(), 1)
})
