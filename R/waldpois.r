#' @title Wald Test Statistic
#'
#' @description Computes the Wald test statistic for the null hypothesis
#' H0: theta = theta_0 where theta is mean of a Poisson distribution.
#' This is discussed in Exercise 6.3.17 on page 386 of HMC.
#'
#' @param x a vector of numeric elements, the sample.
#' @param theta0 a vector of numeric elements, each element is a mean you are checking
#' the set against.
#'
#' @examples x <- c(3.5, -6, 5, 4.444, 10)
#' theta <- 23
#' waldpois(x, theta)
#'
#' @return this function returns a floating point number for each element, the test
#' statistic for each element in theta0
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @details this function uses the following mathematical function:
#' (sqrt(length(x) / mean(x)) * (mean(x) - theta0))^2
#'
#' @export waldpois
#'

waldpois <- function(x, theta0) {
  # argument checking
  errors <- makeAssertCollection()
  # argument 1 x
  errors$push(is_numvector(x, 1))
  errors$push(has_nonan(x, 1))
  errors$push(has_noinf(x, 1))
  reportAssertions(errors)
  errors$push(is_nonzero(mean(x), 1, "argument 1 must have a mean that is not zero"))
  # argument 2 theta0
  errors$push(is_numvector(theta0, 2))
  errors$push(has_nonan(theta0, 2))
  errors$push(has_noinf(theta0, 2))
  reportAssertions(errors)
  # function starts
  n <- length(x)
  th <- mean(x)
  tst <- (sqrt(n/th) * (th - theta0))^2
  return(tst)
}
