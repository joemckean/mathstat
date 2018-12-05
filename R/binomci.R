#' @title Binomial distribution Confidence Interval
#'
#' @description This function iteratively solves for the upper and lower
#' confidence interval bounds for the probaility of success for a binomial sample.
#'
#' @param s an integer the number of successes
#' @param n an integer the the number of trials (zero or more)
#' @param theta1 a number, the lower bracket probability of success for each trial
#' @param theta2 a floading point number, the upper bracket probability of success for each trial, must be larger than theta1
#' @param value a number, the target distribution function
#' @param maxstp an integer default is 100, the amount of times the solution is narrowed down
#' @param eps a number default is .00001, the smallest difference in theta1 and theta2
#'
#' @examples s <- 17
#' n <- 30
#' theta1 <- .4
#' theta2 <- .45
#' value <- .95
#' binomci(s, n, theta1, theta2, value)
#'
#' @return a list with solution and valatsol (value at solution)
#' @return solution a number, the actual confidence interval
#' @return valatsol a number, the actual distribution function the solution is found at
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export binomci
#'
#' @importFrom checkmate makeAssertCollection reportAssertions
#' @import stats
#'
binomci <- function(s, n, theta1, theta2, value, maxstp = 100, eps = 1e-05) {
  
  # checking arguments
  errors <- makeAssertCollection()
  # argument 1 s
  errors$push(has_nonan(s, 1))
  errors$push(has_noinf(s, 1))
  reportAssertions(errors)
  errors$push(is_positive(s, 1))
  errors$push(is_integer(s, 1))
  errors$push(is_numeric(s, 1))
  # argument 2 n
  errors$push(has_nonan(n, 2))
  errors$push(has_noinf(n, 2))
  reportAssertions(errors)
  errors$push(is_positive(n, 2))
  errors$push(is_integer(n, 2))
  errors$push(is_numeric(n, 2))
  # argument 3 theta1
  errors$push(has_nonan(theta1, 3))
  reportAssertions(errors)
  errors$push(is_positive(theta1, 3))
  errors$push(is_inrange(theta1, 3, 0, 1))
  errors$push(is_numeric(theta1, 3))
  # argument 4 theta2
  errors$push(has_nonan(theta2, 4))
  reportAssertions(errors)
  errors$push(is_positive(theta2, 4))
  errors$push(is_inrange(theta2, 4, 0, 1))
  errors$push(is_numeric(theta2, 4))
  # argument 3 and 4 theta1 and theta2
  errors$push(is_smaller(theta1, theta2, 3, 4))
  # argument 5 value
  errors$push(has_nonan(value, 5))
  reportAssertions(errors)
  errors$push(is_positive(value, 5))
  # argument 6 maxstp
  errors$push(has_nonan(maxstp, 6))
  reportAssertions(errors)
  errors$push(is_positive(maxstp, 6))
  errors$push(is_integer(maxstp, 6))
  # argument 7 eps
  errors$push(has_nonan(eps, 7))
  reportAssertions(errors)
  errors$push(is_positive(eps, 7))
  reportAssertions(errors)
  # if all above are ok we can check argumetn 5 value for range
  errors$push(is_inrange(value, 5, pbinom(s, n, theta2), pbinom(s, 
    n, theta1)))
  reportAssertions(errors)
  # function starts
  y1 <- pbinom(s, n, theta1)
  y2 <- pbinom(s, n, theta2)
  # loop maxstp times
  istep <- 0
  while (istep < maxstp) {
    istep <- istep + 1
    theta3 <- (theta1 + theta2)/2
    y3 <- pbinom(s, n, theta3)
    # if y3 is larger than value make theta3 = theta1 and y3 = y1
    # other wise make theta3 = theta2 and y3 = y2
    if (y3 > value) {
      theta1 <- theta3
      y1 <- y3
    } else {
      theta2 <- theta3
      y2 <- y3
    }
    # if theta1 - theta2 is less than eps end loop
    if (abs(theta1 - theta2) < eps) {
      istep <- maxstp
    }
  }
  # return theta3 and y3
  return(list(solution = theta3, valatsol = y3))
}
