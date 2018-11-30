#' @title Density of Log Gamma Distribution
#'
#' @description this function calculates density of log gamma distribution.
#'
#' @param x a numeric Vector. All components are greater than 1.
#' @param alpha a positive non-zero number corresponds to the shape.
#' @param beta a positive non-zero number corresponds to the rate.
#'
#' @return The density of the log gamma distribution
#'
#' @references Hogg, R. McKean, J. Craig, A (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson
#'
#' @examples
#' dloggamma(5, 1, 1)
#'
#' @details f(x) = (1/(gamma(alpha)*beta^alpha))*x^(-(1+beta)/beta)*(log x)^(alpha-1)
#'
#' @export dloggamma
#'

dloggamma <- function(x, alpha, beta) {
	# checking arguments
	errors <- makeAssertCollection()
	# first argument x
	errors$push(has_nonan(x, 1))
  errors$push(is_numvector(x, 1))
	errors$push(is_vecxrange(x, 1, 1, Inf))
	# second argument alpha
	errors$push(has_nonan(alpha, 2))
	errors$push(has_noinf(alpha, 2))
	errors$push(is_oneelement(alpha, 2))
	reportAssertions(errors)
	errors$push(is_numeric(alpha, 2))
	errors$push(is_nonzero(alpha, 2))
	errors$push(is_positive(alpha, 2))
	# third argument beta
	errors$push(has_nonan(beta, 3))
	errors$push(has_noinf(beta, 3))
	errors$push(is_oneelement(beta, 3))
	reportAssertions(errors)
	errors$push(is_numeric(beta, 3))
	errors$push(is_nonzero(beta, 3))
	errors$push(is_positive(beta, 3))
	reportAssertions(errors)
  # start function
  p1 <- 1 / (gamma(alpha) * beta^alpha)
  p2 <- x^(-(beta + 1) / beta) * (log(x))^(alpha - 1)
  # combine both parts of the Mathmatical functions
  dloggamma <- p1 * p2
  # return output of function
  return(dloggamma)
}
