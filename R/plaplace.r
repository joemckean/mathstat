#' @title Laplace CDF
#'
#' @description The function plaplace returns the value of the cdf of
#' the standard Laplace distribution which is discussed on page
#' 77 of the text (HMC).
#'
#' @param x a vector of floating point numbers between -infty and +infinity.
#'
#' @return returns the corresponding vector of probabilities (values of the cdf)
#' for the standard laplace distribution.
#'
#' @examples x <- c(2.2, 0.93, -0.28)
#' plaplace(x)
#' ## [1] 0.9445984 0.8027231 0.3778919
#' ## first element in ps, the second element corespomds to the third element in
#' ## ps, and the third corespomds to the second element in ps becaue the order
#' ## they are reported in
#' ps <- .9750
#' plaplace(ps)
#' ## [1] 2.995732
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @details the cdf is:
#' F(x) =  (1/2)*exp(x)   x < 0, OR
#'         1 - (1/2)*exp(-x)   x > 0
#'
#' @export plaplace
#'

plaplace <- function(x) {

  # checking argument
#j  errors <- makeAssertCollection()
  # argument 1: x
 #j errors$push(has_nonan(x, 1))
#j  reportAssertions(errors)
#j  errors$push(is_numeric(x, 1))
#j  errors$push(has_noinf(x, 1))
  # argument check results
#j  reportAssertions(errors)
  # function starts
  arx <- order(x)
  rx <- rank(x,ties.method = c("first"))
  x <- x[arx]
  low <- x[x < 0.0]
  high <- x[x >= 0.0]
  lowp <- exp(low)/2
  highp <- 1 - (1/2)*exp(-high)
  plaplace <- c(lowp, highp)
  plaplace <- plaplace[rx]
  return(plaplace)
}
