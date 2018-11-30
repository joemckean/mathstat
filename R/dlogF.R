#' @title Density of Log Distribution
#'
#' @description Computes the probability density function (PDF) of a log F random variable.
#' The PDF is given in expression 1.7.9 on page 52 of the book.
#'
#' @param x Real number (or vector of numbers) where -inf < x < inf.
#' In R, these limits are determined by data limitations.
#' Thus we get the following piecewise function,
#' ```
#'            \{   0,          x <= -746
#' dlogF(x) = \{ pdf,  -745 <= x <=  589
#'            \{   0,   590 <= x <=  709
#'            \{ NaN,          x >=  710
#' ```
#' @return PDF (single value or vector based on input) of X.
#'
#' @references Hogg, R. McKean, J. Craig, A (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson
#'
#' @examples
#' pdf <- dlogF(0)
#' pdf <- dlogF(589)
#' pdf <- dlogF(3.14159)
#' pdf <- dlogF(real_number_variable_within_specified_limits)
#'
#' # Example where input is a vector of numbers to be passed into function.
#' # Each number in this input vector is evaluated independently.
#' v <- c(1, 2, 3, 4, 5, 10, 20.5)
#' pdf_vec <- dlogF(v)
#'
#' @export dlogF

dlogF <- function(x) {
	# INPUT VALIDATION
	errors <- makeAssertCollection()
	# argument 1: x
	errors$push(has_nonan(x, 1))
	reportAssertions(errors)

	errors$push(is_numeric(x, 1))
	errors$push(has_noinf(x, 1))
	errors$push(is_vecxrange(x, 1, -746, 710)) # Boundaries for input
	reportAssertions(errors)

	# FUNCTION BEGINS

	# Piecewise of exp() function:
	#			{ 0,	x <= -746
	# exp(x) = 	{ n,    -745 <= x <= 709
	#			{ inf,  710 <= x
	options(digits=22)
	pdf <- exp(x) / (1 + 5 * exp(x))^(1.2)
	return(pdf)
}
