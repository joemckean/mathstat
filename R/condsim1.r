#' @title Observation Generator
#'
#' @description this algorithm generates observations from the probability
#' density function
#'
#' @param nsims a positive non-zero number, the amout of times you want it
#' to generate an observation
#'
#' @return a list of numeric observations
#'
#' @examples nsims <- 5
#' obs <- condsim1(nsims)
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @details observation is generated with this Mathmatical function:
#' log(1-runif(1))+.5*log(1-runif(1))
#'
#' @export condsim1
#'

condsim1 <- function(nsims) {
  # checking arguments
  errors <- makeAssertCollection()
  # first argument nsims
  errors$push(is_oneelement(nsims, 1))
  errors$push(has_nonan(nsims, 1))
  reportAssertions(errors)
  errors$push(is_integer(nsims, 1))
  errors$push(is_positive(nsims, 1))
  errors$push(is_nonzero(nsims, 1))
  errors$push(is_noninf(nsims, 1))
  reportAssertions(errors)
  # function start create an list with nsims places
  collect <- rep(0, nsims)
  # generate observation nsims times
  for (i in 1:nsims) {
    y <- -0.5 * log(1 - runif(1))
    collect[i] <- -log(1 - runif(1)) + y
  }
  return(collect)
}
