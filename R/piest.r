#' @title Estimation of Pi Using Binomial Experiment
#'
#' @description Uses iterative methods to estimate pi with standard error for
#' the simulation discussed in Example 4.8.1 on page 293 of the book.
#'
#' @param n Iterations (number of simulations) to repeat.
#'
#' @return Tuple containing estimate value of pi and standard error.
#'
#' @references Hogg, R. McKean, J. Craig, A (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson
#'
#' @examples
#' # Example where parameters are passed in as variables.
#' n <- 1000
#' result <- piest(n)
#'
#' # Example where parameters are passed in as values.
#' result <- piest(1000)
#'
#' @export piest

piest <- function(n) {
  
  # INPUT VALIDATION
  errors <- makeAssertCollection()
  # argument 1: n
  errors$push(has_nonan(n, 1))
  errors$push(is_oneelement(n, 1))
  reportAssertions(errors)
  
  errors$push(is_numeric(n, 1))
  errors$push(has_noinf(n, 1))
  errors$push(is_positive(n, 1))
  errors$push(is_nonzero(n, 1))
  reportAssertions(errors)
  
  # FUNCTION BEGINS
  
  # Generate n random deviates for uniform distribution between 0 and 1.
  u1 <- runif(n)
  u2 <- runif(n)
  
  cnt <- rep(0, n)  # Create array of n zeros
  # Prob of U1,U2 lying in unit cirle is pi/4 See ex4.8.1 X = 1 if U1^2+U2^2 < 1 else X = 0 thus
  # mean of X is mu = pi/4
  
  chk <- u1^2 + u2^2 - 1
  cnt[chk < 0] <- 1
  estimate <- mean(cnt)
  standard_error <- 4 * sqrt(estimate * (1 - estimate)/n)
  estimate <- 4 * estimate
  return(list(estimate = estimate, standard_error = standard_error))
}
