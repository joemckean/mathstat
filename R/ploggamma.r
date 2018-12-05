#' @title PDF of Mixture Distribution
#'
#' @description Computes the PDF of the loggamma of the continuous-type random variable X given the following:
#' P(a < x < b) = definite integral from z to b of (f(x)dx)
#' f(x) is the density
#' See Remark 3.7.1 on page 219 of the book.
#'
#' @param x Vector of quantiles used in pgamma() function. All components are >= 1.
#'
#' @param alpha Shape value used in pgamma() function. Must be positive.
#'
#' @param beta Rate value used in pgamma() function. Cannot be zero.
#'
#' @return Distribution function from pgamma() function.
#'
#' @references Hogg, R. McKean, J. Craig, A (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson
#'
#' @examples
#' # Example where parameters are passed in as variables.
#' x <- 10
#' alpha <- 1
#' beta <- 1
#' result <- ploggamma(x, alpha, beta)
#'
#' # Example where parameters are passed in as values.
#' result <- ploggamma(10, 1, 1)
#'
#' @export ploggamma

ploggamma <- function(x, alpha, beta) {
  
  # INPUT VALIDATION
  errors <- makeAssertCollection()
  # argument 1: x
  errors$push(has_nonan(x, 1))
  reportAssertions(errors)
  
  errors$push(is_numeric(x, 1))
  errors$push(is_vecinrange(x, 1, 1, Inf))  #elements >= 1
  errors$push(has_noinf(x, 1))
  reportAssertions(errors)
  
  # argument 2: alpha
  errors$push(has_nonan(alpha, 2))
  reportAssertions(errors)
  
  errors$push(is_numeric(alpha, 2))
  errors$push(is_positive(alpha, 2))
  errors$push(is_oneelement(alpha, 2))
  errors$push(has_noinf(alpha, 2))
  reportAssertions(errors)
  
  # argument 3: beta
  errors$push(has_nonan(beta, 3))
  reportAssertions(errors)
  
  errors$push(is_numeric(beta, 3))
  errors$push(is_positive(beta, 3))
  errors$push(is_nonzero(beta, 3))
  errors$push(is_oneelement(beta, 3))
  errors$push(has_noinf(beta, 3))
  reportAssertions(errors)
  
  # FUNCTION BEGINS
  
  ploggamma <- pgamma(log(x), alpha, 1/beta)
  
  return(ploggamma)
}
