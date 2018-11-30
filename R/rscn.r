#' @title random sample from contaminatied normal distribution
#'
#' @description simulates a random sample of size n drawn from a skewed contaminated normal
#' distribution with percent contamination eps, standard deviation ratio
#' sigma_c and mean mu_c.
#'
#' @param n an integer, the size of the sample drawn from the distribution
#' @param eps a number, epsilon usually between 0 and 1
#' @param sd a number, the standard deviation should be greater than or equal to 1
#' @param mu a number, the mean
#'
#' @examples n <- 5
#' eps <- .2
#' sd <- 2.4
#' mu <- 0
#' rscn(n, eps, sd, mu)
#'
#' @return a random sample from contaminated normal distribution
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#' @export rscn


rscn <- function(n, eps, sd, mu){
  errors <- makeAssertCollection()
  # checking arguments
  # argument 1 n
  errors$push(has_nonan(n, 1))
  errors$push(has_noinf(n, 1))
  errors$push(is_oneelement(n, 1))
  errors$push(is_numeric(n, 1))
  reportAssertions(errors)
  errors$push(is_nonzero(n, 1))
  errors$push(is_positive(n, 1))
  errors$push(is_integer(n, 1))
  # argument 2 eps
  errors$push(has_noinf(eps, 2))
  errors$push(has_nonan(eps, 2))
  errors$push(is_oneelement(eps, 2))
  errors$push(is_numeric(eps, 2))
  reportAssertions(errors)
  errors$push(is_inrange(eps, 2, 0, 1))
  # argument 3 sd
  errors$push(has_noinf(sd, 3))
  errors$push(has_nonan(sd, 3))
  errors$push(is_oneelement(sd, 3))
  errors$push(is_numeric(sd, 3))
  reportAssertions(errors)
  errors$push(is_inrange(sd, 3, 1, Inf))
  # argument 4 mu
  errors$push(has_noinf(mu, 4))
  errors$push(has_nonan(mu, 4))
  errors$push(is_oneelement(mu, 4))
  errors$push(is_numeric(mu, 4))
  reportAssertions(errors)
  # function starts
  x1 <- rnorm(n)
  x2 <- rnorm(n, mu, sd)
  b1 <- rbinom(n, 1, eps)
  rscn <- x1 * (1 - b1) + b1 * x2
  return(rscn)
}
