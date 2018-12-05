#' @title Multinomial Trial Simulation
#'
#' @description Computes the result of a random multinomial trial based on the inputted
#' probability of outcome for each k level.
#'
#' @param p A vector containing the probability of outcome for k levels of a random
#' multinomial trial
#'
#' @details An illustration of this function's use can be found in Exercise 3.1.19
#' on page 165.
#'
#' @seealso psum() for ...
#'
#' @return \code{multitrial} contains the outcome of one event for a random multinomial trial.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' ps <- c(0.3, 0.2, 0.2, 0.2, 0.1)
#'
#' # Performing 10 random trials if ps <- c(0.3, 0.2, 0.2, 0.2, 0.1)
#' trials <- 1:10
#' result <- rep(0, length(trials))
#'
#' for (i in 1:length(trials)) {
#'     result[i] <- multitrial(ps)
#' }
#'
#' # Result of 10 random trials
#' result
#'
#' @export multitrial

multitrial <- function(p) {
  
  # checking arguments
  errors <- makeAssertCollection()
  # argument 1 p
  errors$push(is_manyelement(p, 1))
  errors$push(is_numeric(p, 1))
  errors$push(has_nonan(p, 1))
  errors$push(has_noinf(p, 1))
  # argument check results
  reportAssertions(errors)
  
  # Edge case checks for individual elements
  for (i in 1:length(p)) {
    if ((p[i]) < 0 || p[i] > 1) {
      stop(gettext("input vector must contain values between zero and one"))
    } else {
      next
    }
  }
  
  # Error handling cumulative property of 'p'
  if (sum(p) < 1 || sum(p) > 1) {
    stop(gettext("elements of input value 'p' must sum to one"))
  }
  
  # Function starting position
  pr <- c(0, psum(p))
  
  r <- runif(1)
  
  ic <- 0
  
  j <- 1
  
  while (ic == 0) {
    if ((r > pr[j]) && (r <= pr[j + 1])) {
      multitrial <- j
      ic <- 1
    }
    
    j <- j + 1
  }
  
  return(multitrial)
}
