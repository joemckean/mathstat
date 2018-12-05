#' @title Power Function Of The Two Sample Test
#'
#' @description computes power function for two-sample t-testfor the
#' alternative mu1 > mu2. This test is discussed in Example 4.6.2,
#' page 278. Power functions for two-sample t-tests are discussed in
#' Example 8.3.3 on page 493.
#'
#' @param n a number, a sample size of first sample related to mu1
#' @param m a number, a sample size of second sample related to mu2
#' @param Delta a number, Delta = mu1 - mu2 is the
#' argument for the power function.
#' @param sig a number, common standard deviation of the 2 populations
#' @param alpha a number, the significance level of the test
#'
#' @return a number, gamma the power of the t-test to dectect delta
#'
#' @examples n <- 20
#' m <- 15
#' Delta <- 5
#' sig <- 10
#' alpha <- 0.05
#' tpower2g2(n, m, Delta, sig, alpha)
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export tpower2g2
#'

tpower2g2 <- function(n, m, Delta, sig, alpha) {
  # checking arguments
  errors <- makeAssertCollection()
  # argument 1 n
  errors$push(is_numeric(n, 1))
  errors$push(has_noinf(n, 1))
  errors$push(has_nonan(n, 1))
  reportAssertions(errors)
  errors$push(is_positive(n, 1))
  # argument 2 m
  errors$push(has_noinf(m, 2))
  errors$push(has_nonan(m, 2))
  errors$push(is_numeric(m, 2))
  reportAssertions(errors)
  errors$push(is_positive(m, 2))
  errors$push(is_inrange(n + m - 2, 2, 1e-08, Inf, "argument 1 + argument 2 must be greater than 2"))
  # argument 3 delta
  errors$push(has_nonan(Delta, 3))
  errors$push(has_noinf(Delta, 3))
  errors$push(is_numeric(Delta, 3))
  # argument 4 sig
  errors$push(has_noinf(sig, 4))
  errors$push(has_nonan(sig, 4))
  reportAssertions(errors)
  errors$push(is_nonzero(sig, 4))
  errors$push(is_positive(sig, 4))
  # argument 5 alpha
  errors$push(has_noinf(alpha, 5))
  errors$push(has_nonan(alpha, 5))
  errors$push(is_numeric(alpha, 5))
  reportAssertions(errors)
  errors$push(is_xrange(alpha, 5, 0, 1))
  reportAssertions(errors)
  # function starts
  delta <- sqrt(n * m/(m + n)) * (Delta/sig)
  df <- n + m - 2
  tc <- qt(1 - alpha, df)
  gammas <- 1 - pt(tc, df, ncp = delta)
  return(gammas)
}
