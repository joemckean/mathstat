context("betaplts")
library(visualTest)
png("../test_img/betapltstest.png")
betaplts()
dev.off()

test_that("output is correct", {
  finger <- getFingerprint(file = "../test_img/betapltstest.png")
  expect_true(isSimilar(file = "../test_img/betapltskey.png", finger, threshold = 25))
})
