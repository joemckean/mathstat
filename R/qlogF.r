#' @title Quantile of Log Distribution
#'
#' @description Computes the quantiles of the distribution of a log F random variable.
#' The PDF is given in expression 1.7.9 on page 52 of the book.
#'
#' @param x Real number(or vector of numbers) where 0 < x < 1.
#' CDF of X is F(x) = 1 - (1+5e^(-x))^(-0.1), -inf < x < inf.
#' # X is a real number with the following piecewise function:
#' ```
#'            \{  NaN,      x < 0
#'            \{ -Inf,      x = 0
#' qlogF(x) = \{    n,  0 < x < 1
#'            \{  Inf,      x = 1
#'            \{  NaN,      x > 1
#' ```
#' @return Inverse of the CDF (single value or vector based on input) of X.
#'
#' @references Hogg, R. McKean, J. Craig, A (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson
#'
#' @examples
#' inverse_cdf <- qlogF(0.1)
#' inverse_cdf <- qlogF(0.9)
#'
#' @export qlogF

qlogF <- function(x){
	# INPUT VALIDATION
	errors <- makeAssertCollection()
	# argument 1: x
	errors$push(has_nonan(x, 1))
	reportAssertions(errors)

	errors$push(is_numeric(x, 1))
	errors$push(has_noinf(x, 1))
	errors$push(is_vecxrange(x, 1, 0, 1)) # Bounds specified in manpage
	reportAssertions(errors)

	# FUNCTION BEGIN

	inverse_cdf <- log(0.2 * ((1 - x)^(-5) - 1))

	return(inverse_cdf)
}
