#' @title Graphs a Two Sample T-Test
#'
#' @description Graphs the power of the one-sample, two-sided $t$-test
#' for the mean. The hypothese are:\cr
#' H0: mu = mu0 vs. H1: mu != mu0.\cr
#' See Example 8.3.2 on page 492 for discussion of this test andits power.
#' Exercise 8.3.5 continues the discussion.
#'
#' @param mu0 a number, null population mean null value
#' @param sig a number, population standard deviation usually greater than 1
#' @param n an integer, sample size
#' @param alpha a number, the signifigance level, defaults to 0.05
#' @param byv a number, increment in sequence for mu 1, defaults to 0.1
#'
#' @return gammas vector of powers from power function
#'
#' @examples mu0 <- 50
#' sig <- 10
#' n <- 25
#' alpha <- .04
#' byv <- .15
#' tpowerg2(mu0, sig, n, alpha, byv)
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export tpowerg2
#'

tpowerg2 <- function(mu0, sig, n, alpha = 0.05, byv = 0.1) {
  # checking arguments
  errors <- makeAssertCollection()
  # argument 1 mu0
  errors$push(has_nonan(mu0, 1))
  errors$push(has_noinf(mu0, 1))
  errors$push(is_numeric(mu0, 1))
  errors$push(is_oneelement(mu0, 1))
  # argument 2 sig
  errors$push(has_nonan(sig, 2))
  errors$push(is_numeric(sig, 2))
  errors$push(is_oneelement(sig, 2))
  reportAssertions(errors)
  errors$push(is_xrange(sig, 2, 0, Inf))
  # argument 3 n
  errors$push(has_nonan(n, 3))
  reportAssertions(errors)
  errors$push(is_xrange(n, 3, 1, Inf))
  errors$push(is_integer(n, 3))
  # argument 4 alpha
  errors$push(has_nonan(alpha, 4))
  errors$push(has_noinf(alpha, 4))
  errors$push(is_numeric(alpha, 4))
  reportAssertions(errors)
  errors$push(is_xrange(alpha, 4, 0, 1))
  # argument 5 byv
  errors$push(has_nonan(byv, 5))
  errors$push(has_noinf(byv, 5))
  errors$push(is_numeric(byv, 5))
  reportAssertions(errors)
  errors$push(is_xrange(byv, 5, 0, Inf))
  reportAssertions(errors)
  # function starts
  min1 <- mu0 - 4 * sig/sqrt(n)
  max1 <- mu0 + 4 * sig/sqrt(n)
  mu1 <- seq(min1, max1, byv)
  delta <- (mu1 - mu0)/(sig/sqrt(n))
  tc <- qt(1 - (alpha/2), n - 1)
  gammas <- 1 - pt(tc, n - 1, ncp = delta) + pt(-tc, n - 1, ncp = delta)
  plot(gammas ~ mu1, pch = " ", xlab = expression(mu[1]), ylab = expression(gamma))
  lines(gammas ~ mu1)
  return(gammas)
}
