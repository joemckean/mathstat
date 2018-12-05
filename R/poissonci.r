#' @title Confidence Interval for mean of of a Poisson Distribution
#'
#' @description This function iteratively solves for the upper and lower
#' confidence interval bounds for the probability of success for a poisson
#' sample.
#'
#' @param s a number, the number of events that have occured calculted as n * xbar
#' @param n a number, the size
#' @param theta1 a number n*theta1 is a mean and the lower bracket for the solution, must be positive.
#' @param theta2 a number n*thata2 is a mean and the upper bracket for the solution, must be larger than theta1
#' @param value a number the target distribution function
#' @param maxstp an integer default is 100, the amount of times the solution is narrowed down
#' @param eps a number default is .00001, the smallest difference in theta1 and theta2 as they are updated
#'
#' @return a list with solution and valatsol (value at solution)
#' @return solution a floating point number, the actual confidence interval
#' @return valatsol a floating point number, the actual distribution function solution is found at
#'
#' @examples s <- 125
#' n <- 25
#' theta1 <- 5.5
#' theta2 <- 6
#' value <- .05
#' poissonci(s, n, theta1, theta2, value)
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export poissonci
#'
poissonci <- function(s, n, theta1, theta2, value, maxstp = 100, eps = 1e-05) {
  # checking arguments
  errors <- makeAssertCollection()
  # argument 1 s
  errors$push(has_nonan(s, 1))
  errors$push(has_noinf(s, 1))
  errors$push(is_numeric(s, 1))
  reportAssertions(errors)
  errors$push(is_positive(s, 1))
  # argument 2 n
  errors$push(is_numeric(n, 2))
  errors$push(has_nonan(n, 2))
  errors$push(has_noinf(n, 2))
  reportAssertions(errors)
  errors$push(is_nonzero(n, 2))
  errors$push(is_positive(n, 2))
  # argument 3 theta1
  errors$push(is_numeric(theta1, 3))
  errors$push(has_nonan(theta1, 3))
  errors$push(has_noinf(theta1, 3))
  reportAssertions(errors)
  errors$push(is_positive(theta1, 3))
  # argument 4 theta2
  errors$push(is_numeric(theta2, 4))
  errors$push(has_nonan(theta2, 4))
  errors$push(has_noinf(theta2, 4))
  reportAssertions(errors)
  errors$push(is_positive(theta2, 4))
  # argument 3 and 4 theta1 and theta2
  errors$push(is_smaller(theta1, theta2, 3, 4))
  # argument 5 value
  errors$push(is_numeric(value, 5))
  errors$push(has_nonan(value, 5))
  errors$push(has_noinf(value, 5))
  reportAssertions(errors)
  errors$push(is_positive(value, 5))
  # argument 6 maxstp
  errors$push(is_numeric(maxstp, 6))
  errors$push(has_nonan(maxstp, 6))
  errors$push(has_noinf(maxstp, 6))
  reportAssertions(errors)
  errors$push(is_positive(maxstp, 6))
  errors$push(is_integer(maxstp, 6))
  errors$push(is_nonzero(maxstp, 6))
  # argument 7 eps
  errors$push(is_numeric(eps, 7))
  errors$push(has_nonan(eps, 7))
  reportAssertions(errors)
  errors$push(is_positive(eps, 7))
  reportAssertions(errors)
  # if all above are ok we can check argumetn 5 value for range
  errors$push(is_inrange(value, 5, ppois(s, n * theta2), ppois(s, 
    n * theta1)))
  reportAssertions(errors)
  y1 <- ppois(s, n * theta1)
  y2 <- ppois(s, n * theta2)
  # loop starts
  istep <- 0
  while (istep < maxstp) {
    istep <- istep + 1
    theta3 <- (theta1 + theta2)/2
    y3 <- ppois(s, n * theta3)
    if (y3 > value) {
      theta1 <- theta3
      y1 <- y3
    } else {
      theta2 <- theta3
      y2 <- y3
    }
    if (abs(theta1 - theta2) < eps) {
      istep <- maxstp
    }
  }
  list(solution = theta3, valatsol = y3)
}
