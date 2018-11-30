#' @title Poisson Random Sample Generator
#'
#' @description This function creates a random sample of n elements using the
#' poisson distribution.
#'
#' @param n a positive non-zero integer the, size of sample
#' @param lambda a positive non-zero floating point number, The mean of the poisson distribution
#'
#' @details Simulates the Poisson Processes in Example 4.8.2 on page 295 of HMC
#' (2018).
#'
#' @return a poisson random sample
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to Mathematical
#' Statistics, 8th Ed. Boston: Pearlson
#'
#' @examples
#' n <- 10
#' lambda <- 25.2
#' poisrand(n, lambda)
#' mean(poisrand(10000, lambda))
#'
#' @export poisrand
#'
#' @import stats

poisrand <-function(n,lambda){
	# checking arguments
	errors <- checkmate::makeAssertCollection()
	# argument 1 n
	errors$push(is_positive(n, 1))
	errors$push(is_nonzero(n, 1))
	errors$push(is_integer(n, 1))
	errors$push(is_oneelement(n, 1))
	# argument 2 lambda
	errors$push(is_numeric(lambda, 2))
	errors$push(is_positive(lambda, 2))
	errors$push(is_oneelement(lambda, 2))
	errors$push(is_nonzero(lambda, 2))
	checkmate::reportAssertions(errors)
	# function starts
  poisrand = rep(0,n)
  # loop n times
  for(i in 1:n) {
    xt <- 0
    t <- 0
    # loop until t is greater than or equal to 1
    while(t < 1) {
      x <- xt
      y <- -(1/lambda)*log(1-runif(1))
      t <- t + y
      xt <- xt + 1
      }
    poisrand[i] <- x
    }
  return(poisrand)
}
