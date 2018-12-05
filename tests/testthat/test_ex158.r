context("ex158 test")
library(visualTest)
png("../test_img/ex158test.png")
ex158()
dev.off()

test_that("output", {
  finger <- getFingerprint(file = "../test_img/ex158key.png")
  expect_true(isSimilar(file = "../test_img/ex158test.png", finger, 
    threshold = 25))
})
