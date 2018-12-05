#' @title Power of the One Sample t-Test
#'
#' @description Computes a graph of the power curve of a one sample t-test
#' of mu=mu0 vs mu!=mu0 as shown in Example 8.3.2 on page 492 of the book.
#' This function assumes that X_1,...,X_n is a random sample on X that has a
#' N(mu, sigma^2) distribution. We are testing H_0: mu=mu_0 versus
#' H_1: mu != mu_0, where mu_0 is specified. Thus the likelihood ratio test
#' statistic is
#'
#' \eqn{t(X_1,...,X_n)}
#'
#' \eqn{= \sqrt(n)(Xbar - \mu_0)/
#' \sqrt((\sum(1 -> n)((X_i - Xbar)^2)/(n - 1)))}
#'
#' \eqn{= (\sqrt(n)(Xbar - \mu_0)/\sigma) / (\sqrt( \sum(1 -> n)(X_1 - Xbar)^2 /
#' \(\sigma^2(n-1))))}
#'
#' The hypothesis H_0 is rejected at level \eqn{\alpha} if
#' \eqn{|t| \ge t_\alpha / (2,n-1)}.
#'
#' See page 492 and Exercise 8.3.5 on page 497 of the book.
#'
#' @param mu0 Null population mean.
#'
#' @param sig Population standard deviation.
#'
#' @param n Sample size.
#'
#' @param alpha Level of significance.
#'
#' @param byv Increment in sequence for creating mu1.
#'
#' @return Vector of powers from power fucntion.
#'
#' @references Hogg, R. McKean, J. Craig, A (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson
#'
#' @examples
#' tpowerg(50, 10, 25)
#' tpowerg(50, 10, 25, 0.04, 0.2)
#'
#' @export tpowerg

tpowerg <- function(mu0, sig, n, alpha = 0.05, byv = 0.1) {
  
  # INPUT VALIDATION
  errors <- makeAssertCollection()
  # argument 1: mu0
  errors$push(has_nonan(mu0, 1))
  reportAssertions(errors)
  
  errors$push(is_numeric(mu0, 1))
  errors$push(is_nonzero(mu0, 1))
  errors$push(is_positive(mu0, 1))
  errors$push(is_oneelement(mu0, 1))
  errors$push(has_noinf(mu0, 1))
  reportAssertions(errors)
  
  # argument 2: sig
  errors$push(has_nonan(sig, 2))
  reportAssertions(errors)
  
  errors$push(is_numeric(sig, 2))
  errors$push(is_nonzero(sig, 2))
  errors$push(is_positive(sig, 2))
  errors$push(is_oneelement(sig, 2))
  errors$push(has_noinf(sig, 2))
  reportAssertions(errors)
  
  # argument 3: n
  errors$push(has_nonan(n, 3))
  reportAssertions(errors)
  
  errors$push(is_numeric(n, 3))
  errors$push(is_nonzero(n, 3))
  errors$push(is_positive(n, 3))
  errors$push(is_oneelement(n, 3))
  errors$push(has_noinf(n, 3))
  reportAssertions(errors)
  
  # argument 4: alpha
  errors$push(has_nonan(alpha, 4))
  reportAssertions(errors)
  
  errors$push(is_numeric(alpha, 4))
  errors$push(is_nonzero(alpha, 4))
  errors$push(is_positive(alpha, 4))
  errors$push(is_oneelement(alpha, 4))
  errors$push(has_noinf(alpha, 4))
  reportAssertions(errors)
  
  # argument 5: byv
  errors$push(has_nonan(byv, 5))
  reportAssertions(errors)
  
  errors$push(is_numeric(byv, 5))
  errors$push(is_nonzero(byv, 5))
  errors$push(is_positive(byv, 5))
  errors$push(is_oneelement(byv, 5))
  errors$push(has_noinf(byv, 5))
  reportAssertions(errors)
  
  # FUNCTION BEGINS
  
  fse <- 4 * sig/sqrt(n)
  maxmu <- mu0 + fse
  tc <- qt(1 - (alpha/2), n - 1)
  minmu <- mu0 - fse
  mu1 <- seq(minmu, maxmu, byv)
  delta <- (mu1 - mu0)/(sig/sqrt(n))
  gammas <- 1 - pt(tc, n - 1, ncp = delta) + pt(-tc, n - 1, ncp = delta)
  plot(gammas ~ mu1, pch = " ", xlab = expression(mu[1]), ylab = expression(gamma))
  lines(gammas ~ mu1)
  
  return(gammas)
}
