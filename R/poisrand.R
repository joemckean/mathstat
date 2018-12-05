#' @title Simulating Poisson Processes
#'
#' @description Generate n simulations of a Poisson Distribution. See Example
#' 4.8.2 on page 295 of 8th edition.
#'
#' @param n Iterations to repeat (number of simulations).
#'
#' @param lambda Mean variable in function.
#'
#' @return Vector of realizations from poisson distribution.
#'
#' @examples
#' # Example where parameters are passed in as variables.
#' n <- 1000
#' lambda <- 5
#' result <- poisrand(n, lambda)
#'
#' # Example where parameters are passed in as values.
#' result <- poisrand(1000, 5)
#'
#' @export poisrand

poisrand <- function(n, lambda) {
  
  # INPUT VALIDATION
  errors <- makeAssertCollection()
  # argument 1: n
  errors$push(has_nonan(n, 1))
  errors$push(is_oneelement(n, 1))
  reportAssertions(errors)
  
  errors$push(is_positive(n, 1))
  errors$push(is_nonzero(n, 1))
  errors$push(is_numeric(n, 1))
  errors$push(has_noinf(n, 1))
  reportAssertions(errors)
  
  # argument 2: lambda
  errors$push(has_nonan(lambda, 2))
  errors$push(is_oneelement(lambda, 2))
  reportAssertions(errors)
  
  errors$push(is_positive(lambda, 2))
  errors$push(is_nonzero(lambda, 2))
  errors$push(is_numeric(lambda, 2))
  errors$push(has_noinf(lambda, 2))
  reportAssertions(errors)
  
  # FUNCTION BEGINS
  
  # try(if(lambda < 0) stop('Lambda must be >= 0.'))
  poisrand <- rep(0, n)  # Vector of n number of 0's
  for (i in 1:n) {
    # step 1 in ex4.8.2
    xt <- 0
    t <- 0
    while (t < 1) {
      x <- xt
      y <- -(1/lambda) * log(1 - runif(1))  #step 2
      t <- t + y  #step 3
      xt <- xt + 1  #step 4
    }
    poisrand[i] <- x  #save result into appropriate index in vector
  }
  return(poisrand)
}
