context("binompower")
library(visualTest)
png("../test_img/binompowertest.png")
binompower()
dev.off()

test_that("output", {
    finger <- getFingerprint(file = "../test_img/binompowerkey.png")
    expect_true(isSimilar(file = "../test_img/binompowertest.png",
    					  finger,
    					  threshold=25))
})
