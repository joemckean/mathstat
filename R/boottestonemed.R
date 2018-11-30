#' @title Bootstrap Test One Median
#'
#' @description  On page 311 of HMC (2019), a bootstrap test for a one sample location problem based
#' on the sample mean is presented. For the function, boottestonemed, the sample median is
#' used for the test statistic. The hypotheses tested, are:H 0 : θ = θ 0 versus H a : θ > θ 0 where
#' θ 0 is spectified, (an input value of the functioin). The bootstrapped p-value is returned.
#'
#' @param sample a vector, is the sample
#' @param theta0 a number, the null value of the mean
#' @param b an integer, the number of bootstrap resamples
#'
#' @examples sample <- c(4, 3, 2, 9)
#' theta0 <- .05
#' b <- 5
#' boottestonemed(sample, theta0, b)
#'
#' @return a list with origtest, pvalue, and teststatall
#' @return originaltest is the median of the sample
#' @return pvalue is the bootstrap p-value
#' @return teststatall contains the bootstrap test median
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export boottestonemed
#'

boottestonemed <- function(sample, theta0, b) {
  # checking arguments
	errors <- checkmate::makeAssertCollection()
  # argument 1 sample
	errors$push(has_nonan(sample, 1))
	errors$push(has_noinf(sample, 1))
	errors$push(is_numvector(sample, 1))
	# argument 2 theta0
	errors$push(is_numeric(theta0, 2))
	errors$push(has_nonan(theta0, 2))
	errors$push(has_noinf(theta0, 2))
	# argument 3 b
	errors$push(has_nonan(b, 3))
	errors$push(has_noinf(b, 3))
	reportAssertions(errors)
	errors$push(is_integer(b, 3))
	errors$push(is_positive(b, 3))
	checkmate::reportAssertions(errors)
  # start function
  n <- length(sample)
  v <- median(sample)
  z <- sample - median(sample) + theta0
  counter <- 0
  teststatall <- rep(0, b)
  # loop b times
  for (i in 1:b) {
    samplestar <- sample(z, n, replace = TRUE)
    vstar <- median(samplestar)
    # if vstar is greater than or equal to v increment counter
    if (vstar >= v) {
      counter <- counter + 1
    }
    teststatall[i] <- vstar
  }
  pvalue <- counter / b
  return(list(origtest = v, pvalue = pvalue, teststatall = teststatall))
}
