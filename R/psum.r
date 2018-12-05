#' @title Probability Outcomes for a Multinomial Trial
#'
#' @description \code{psum} is an auxiliary function used in the function \code{multitrial}
#' which generates a random trial from a multinormal distribution. If there are k categories
#' for a multinomial with respective probabilities in the k by 1 vector ps then
#' psum(ps) returns the accumulating probabilities {p_1, p_1+p_2, ..., 1}.
#'
#' @param ps k by 1 vector containing probabilities for k possible outcomes
#'
#' @details An illustration of this function's use can be found in Exercise 3.1.19
#' on page 165.
#'
#' @seealso multitrial() for more on computing multinomial trials
#'
#' @return \code{psum} contains k parameters of a multinomial distribution
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' ps <- c(0.1, 0.1, 0.2, 0.3, 0.2, 0.1)
#' psum(ps)
#'
#' ps <- c(0.2, 0.2, 0.2, 0.2, 0.2)
#' psum(ps)
#' @export psum

psum <- function(ps) {
  
  # checking arguments
  errors <- makeAssertCollection()
  # argument 1 ps
  errors$push(has_nonan(ps, 1))
  errors$push(has_noinf(ps, 1))
  errors$push(is_numvector(ps, 1))
  errors$push(is_manyelement(ps, 1))
  # argument check results
  reportAssertions(errors)
  
  # Edge case checks for individual elements
  for (i in 1:length(ps)) {
    if ((ps[i]) < 0 || ps[i] > 1) {
      stop(gettext("input vector must contain values between zero and one"))
    } else {
      next
    }
  }
  
  # Function starting position
  p <- 0
  psum <- c()
  
  for (j in 1:length(ps)) {
    p <- p + ps[j]
    psum <- c(psum, p)
  }
  
  return(psum)
}
