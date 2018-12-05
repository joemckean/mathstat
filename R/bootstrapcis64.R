#' @title Percentile Bootstrap Confidence Interval
#'
#' @description This function returns a bootstrap confidence for the maximum likelihood
#' estimator that x is less than or eqal to c discussed in Exercise 6.4.7 on page 395.
#'
#' @param x a random variables with sample size \emph{n}
#' @param c a number, a range value of intrest
#' @param nb a positive integer, the number iteratins to estimate the
#' confidence interval. defaults to 3000
#' @param alp2 a number, decides where the lower and upper bound
#' will be found. defaults to 0.025
#'
#' @examples x <- c(4, -2, 5.43, 10)
#' c <- 5
#' nb <- 3000
#' alp2 <- 0.025
#' bootstrapcis64(x, c, nb, alp2)
#'
#' @return a list containg estimate, lower bound and upper bound
#' @return estimate the estimated probability x <= c
#' @return lower bound, lower bound of the confidance interval
#' @return upper bound, upper bound of the confidence interval
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export bootstrapcis64


bootstrapcis64 <- function(x, c, nb = 3000, alp2 = 0.025) {
  # argument checking
  errors <- makeAssertCollection()
  # checking argument 1 x
  errors$push(is_numvector(x, 1))
  errors$push(is_manyelement(x, 1))
  errors$push(has_noinf(x, 1))
  errors$push(has_nonan(x, 1))
  # checking argument 2 c
  errors$push(is_oneelement(c, 2))
  errors$push(is_numeric(c, 2))
  errors$push(has_nonan(c, 2))
  # checking argument 3 nb
  errors$push(is_oneelement(nb, 3))
  errors$push(has_nonan(nb, 3))
  reportAssertions(errors)
  errors$push(is_integer(nb, 3))
  errors$push(is_nonzero(nb, 3))
  errors$push(is_positive(nb, 3))
  errors$push(is_noninf(nb, 3))
  # checking argument 4 alp2
  errors$push(has_nonan(alp2, 4))
  reportAssertions(errors)
  errors$push(is_xrange(alp2, 4, 0, 1))
  reportAssertions(errors)
  # function starts
  n <- length(x)
  xb <- mean(x)
  sb <- (((n - 1)/n) * var(x))^0.5
  est <- pnorm((c - xb)/sb)
  collest <- c()
  # loop nb times
  for (i in 1:nb) {
    xr <- sample(x, n, replace = TRUE)
    xb <- mean(xr)
    sb <- (((n - 1)/n) * var(xr))^0.5
    estr <- pnorm((c - xb)/sb)
    collest <- c(collest, estr)
  }
  # find lower and upper bounds and return
  colls <- sort(collest)
  cut <- round(nb * alp2)
  lb <- colls[cut]
  ub <- colls[nb - (cut - 1)]
  return(list(estimate = est, lowerbound = lb, upperbound = ub))
  
}
