context('pltlogF test')
library(visualTest)
png("../test_img/pltlogftest.png")
pltlogF()
dev.off()

test_that("output is correct", {
    finger <- getFingerprint(file = "../test_img/pltlogfkey.png")
    expect_true(isSimilar(file = "../test_img/pltlogftest.png",
    					  finger,
    					  threshold=25))
})
