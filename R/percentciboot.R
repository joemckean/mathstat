#' @title Percent Confidence Interval bootstrap
#'
#' @description obtains the percentile confidence interval for the mean.
#'
#' @param sample, the original sample.
#' @param b an integer the desired numbers of bootstraps.
#' @param alpha a number 1 - alpha is the confidence coefficient.
#'
#' @examples sample <- c(4, 3, 2, 3)
#' b <- 20
#' alpha <- .05
#' percentciboot(sample, b, alpha)
#'
#' @return a list with thata, lower, upper, thetastar
#' @return thata is the point estimate.
#' @return lower is the lower end of the percentile confidence interval.
#' @return upper is the upper end of the percentile confidence interval.
#' @return thetastar is the vector of bootstrapped thetastars
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export percentciboot
#'
percentciboot <- function(sample, b, alpha = 0.05) {
  # checking arguments
  errors <- makeAssertCollection()
  # checking argument 1 sample
  errors$push(is_numvector(sample, 1))
  errors$push(has_noinf(sample, 1))
  errors$push(has_nonan(sample, 1))
  # checking argument 2 b
  errors$push(is_oneelement(b, 2))
  errors$push(has_nonan(b, 2))
  reportAssertions(errors)
  errors$push(is_integer(b, 2))
  errors$push(is_inrange(b, 2, 20, Inf))
  errors$push(is_noninf(b, 2))
  # checking argument 3 alpha
  errors$push(has_nonan(alpha, 3))
  reportAssertions(errors)
  errors$push(is_xrange(alpha, 3, 0, 1))
  reportAssertions(errors)
  # start function
  theta <- mean(sample)
  thetastar <- rep(0, b)
  n <- length(sample)
  # loop b times
  for (i in 1:b) {
    samplestar <- sample(sample, n, replace = TRUE)
    thetastar[i] <- mean(samplestar)
  }
  thetastar <- sort(thetastar)
  pick <- round((alpha/2) * (b + 1))
  lower <- thetastar[pick]
  upper <- thetastar[b - pick + 1]
  return(list(theta = theta, lower = lower, upper = upper, thetastar = thetastar))
}
