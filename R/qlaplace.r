#' @title Laplace Quantiles
#'
#' @description The function qlaplace returns the quantiles for
#' the standard Laplace distribution which is discussed on page
#' 77 of the text.   Quantiles for this distribution are
#' discussed on page 260, Example 4.4.6.
#'
#' @param ps a vector of floating point numbers between 0 and 1, of probabilites.
#'
#' @return returns a vector of quantiles for standard laplace distribution. For
#' quantiles with probabilites less than .5 reported first and greater than .5
#' following them.
#'
#' @examples ps <- c(0.22, 0.93, 0.28)
#' qlaplace(ps)
#' ## [1] -0.8209806 -0.5798185  1.9661129 the first element corresponds to the
#' ## first element in ps, the second element corespomds to the third element in
#' ## ps, and the third corespomds to the second element in ps becaue the order
#' ## they are reported in
#' ps <- .9750
#' qlaplace(ps)
#' ## [1] 2.995732
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @details the mathmatical function that the entrys go through are:\cr
#' f(x) = { log(2 * x)\cr
#'        { -log(2 * (1 - x))
#'
#' @export qlaplace
#'

qlaplace <- function(ps) {
	# checking argument
	errors <- makeAssertCollection()
	# argument 1 ps
	errors$push(is_numvector(ps, 1))
	errors$push(is_vecinrange(ps, 1, 0, 1))
	errors$push(has_nonan(ps, 1))
	reportAssertions(errors)
	# function starts
  low <- ps[ps < 0.5]
  high <- ps[ps >= 0.5]
  lowq <- log(2 * low)
  highq <- -log(2 * (1 - high))
  qlaplace <- c(lowq, highq)
  return(qlaplace)
}
