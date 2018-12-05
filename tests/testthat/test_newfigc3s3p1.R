context("newfigc3s3p1")
library(visualTest)
png("../test_img/newfigc3s3p1test.png")
newfigc3s3.1()
dev.off()

test_that("output", {
  finger <- getFingerprint(file = "../test_img/newfigc3s3p1key.png")
  expect_true(isSimilar(file = "../test_img/newfigc3s3p1test.png", 
    finger, threshold = 25))
})
