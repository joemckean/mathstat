context("plotsulfur test")
library(visualTest)
png("../test_img/plotsulfurtest.png")
plotsulfur()
dev.off()

test_that("output is correct", {
    finger <- getFingerprint(file = "../test_img/plotsulfurkey.png")
    expect_true(isSimilar(file = "../test_img/plotsulfurtest.png",
    					  finger,
    					  threshold=10))
})
