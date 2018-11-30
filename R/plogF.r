#' @title Probability of Log Distribution
#'
#' @description Computes the distribution function (CDF) of a log F random variable.
#' The PDF is given in expression 1.7.9 on page 52 of the book.
#'
#' @param x Real number (or vector of numbers) where \eqn{-\infty < x < \infty}.
#' CDF of X is \eqn{F(x) = 1 - (1+5e^(x))^(-0.2)}.
#'
#' @return CDF (single value or vector based on input) of X. \eqn{P(X\le x)}
#'
#' @references Hogg, R. McKean, J. Craig, A (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson
#'
#' @examples
#' cdf <- plogF(1)
#' cdf <- plogF(-1)
#' cdf <- plogF(3.14159)
#'
#' @export plogF

plogF <- function(x){
	# INPUT VALIDATION
	errors <- makeAssertCollection()
	# argument 1: x
	errors$push(has_nonan(x, 1))
	reportAssertions(errors)

	errors$push(is_numeric(x, 1))
	errors$push(has_noinf(x, 1))
	reportAssertions(errors)

	# FUNCTION BEGINS

	cdf <- (1 - (1 + 5 * exp(x))^(-0.2))

	return(cdf)
}

