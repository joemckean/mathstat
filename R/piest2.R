#' @title Estimation of Pi by Monte Carlo Integration
#'
#' @description Uses integration to estimate pi given a sample size.
#' See example 4.8.4 on page 295 and 296 of the book.
#'
#' @param n Sample size.
#'
#' @return Tuple containing estimate value of pi and the standard error.
#'
#' @references Hogg, R. McKean, J. Craig, A (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson
#'
#' @examples
#' # Example where parameters are passed in as variables.
#' n <- 1000
#' result <- piest2(n)
#'
#' # Example where parameters are passed in as values.
#' result <- piest2(1000)
#'
#' @export piest2

piest2 <- function(n){

	# INPUT VALIDATION
	errors <- makeAssertCollection()
	# argument 1: n
	errors$push(has_nonan(n, 1))
	errors$push(is_oneelement(n, 1))
	reportAssertions(errors)

	errors$push(is_numeric(n, 1))
	errors$push(has_noinf(n, 1))
	errors$push(is_positive(n, 1))
	errors$push(is_nonzero(n, 1))
	reportAssertions(errors)

	# FUNCTION BEGINS

	sample <- 4 * sqrt(1 - runif(n)^2)
	estimate <- mean(sample)
	standard_error <- sqrt(var(sample) / n)

	return(list(estimate=estimate,standard_error=standard_error))
}
