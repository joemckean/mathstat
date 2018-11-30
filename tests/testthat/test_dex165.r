context('dex165 test')
samp <- dex165()
eps <- .000001

test_that("output", {
	expect_is(samp, "matrix")
	expect_true(samp['x',1]==0)
	expect_true(samp['x',2]==1)
	expect_true(samp['x',3]==2)
	expect_true(samp['x',4]==3)
	expect_true(samp['x',5]==4)
	expect_true(samp['x',6]==5)
	expect_true(samp['pmf',1]>=.3193094-eps && samp['pmf',1]<=.3193094+eps)
	expect_true(samp['pmf',2]>=.420144-eps && samp['pmf',2]<=.420144+eps)
	expect_true(samp['pmf',3]>=.2073438-eps && samp['pmf',3]<=.2073438+eps)
	expect_true(samp['pmf',4]>=.04784857-eps && samp['pmf',4]<=.04784857+eps)
	expect_true(samp['pmf',5]>=.005148264-eps && samp['pmf',5]<=.005148264+eps)
	expect_true(samp['pmf',6]>=.0002059305-eps && samp['pmf',6]<=.0002059305+eps)
})

