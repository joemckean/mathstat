#' @title Gibbs Sampler with Bayesian Inference
#'
#' @description  This function computes the Gibbs sampler routine as discussed on
#' page 677, but with Bayesian influence integrated. The input parameter alpha
#' is the parameter in the joint distribution of the random vector (X, Y) which
#' is given in the example. The value of m is the number of initial runs of the
#' sampler for achieving equilibrium; then the next (n * m) runs are recorded and returned
#' in the R vectors x2 and y2. The paired items (x2, i) \& (y2, i) are the variates for
#' random vector (X, Y).
#'
#' @param alpha test statistic applied in algorithm
#' @param beta NEED INFO
#' @param nt NEED INFO
#' @param m length of returned variables x1 and y1
#' @param n length of returned variables x2 and y2
#'
#' @return A list of two streams of iid random variables. The first pair is of
#' length \emph{m} and the second of length \emph{n}.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @seealso gibbser2() for more details on the Gibbs sampler routine described on
#' page 677.
#'
#' @examples
#' gibbser3(alpha = 10, beta = 4, nt = 100, m = 3000, n = 6000)
#' gibbser3(alpha = 12, beta = 4, nt = 100, m = 4000, n = 10000)
#'
#' @export gibbser3

gibbser3 <- function(alpha,
					 beta = 0,
					 nt = 0,
					 m = 0,
					 n = 1) {

	# checking arguments
	errors <- makeAssertCollection()
	# argument 1 alpha
	errors$push(has_nonan(alpha, 1))
	errors$push(has_noinf(alpha, 1))
	errors$push(is_nonzero(alpha, 1))
	errors$push(is_positive(alpha, 1))
	errors$push(is_numeric(alpha, 1))
	errors$push(is_oneelement(alpha, 1))
	# argument 2 beta
	errors$push(has_nonan(beta, 2))
	errors$push(has_noinf(beta, 2))
	errors$push(is_nonzero(beta, 2))
	errors$push(is_positive(beta, 2))
	errors$push(is_numeric(beta, 2))
	errors$push(is_oneelement(beta, 2))
	# argument 3 nt
	errors$push(has_nonan(nt, 3))
	errors$push(has_noinf(nt, 3))
	errors$push(is_nonzero(nt, 3))
	errors$push(is_positive(nt, 3))
	errors$push(is_numeric(nt, 3))
	errors$push(is_oneelement(nt, 3))
	# argument 4 m
	errors$push(has_nonan(m, 4))
	errors$push(has_noinf(m, 4))
	errors$push(is_nonzero(m, 4))
	errors$push(is_positive(m, 4))
	errors$push(is_numeric(m, 4))
	errors$push(is_oneelement(m, 4))
	errors$push(is_smaller(m, n, 4, 5))
	# argument 5 n
	errors$push(has_nonan(n, 5))
	errors$push(has_noinf(n, 5))
	errors$push(is_nonzero(n, 5))
	errors$push(is_positive(n, 5))
	errors$push(is_numeric(n, 5))
	errors$push(is_oneelement(n, 5))
	# argument check results
	reportAssertions(errors)

	x0 <- 1

	yc <- rep(0, m + n)
	xc <- c(x0, rep(0, (m - 1) + n))

	for(i in 2:(m + n)) {
		yc[i] <- rbeta(1, xc[i - 1] + alpha, nt - xc[i - 1] + beta)
		xc[i] <- rbinom(1, nt, yc[i])
	}

	y1 <- yc[1:m]
	y2 <- yc[(m+1):(m+n)]

	x1 <- xc[1:m]
	x2 <- xc[(m+1):(m+n)]

	return(list(y1=y1,
				y2=y2,
				x1=x1,
				x2=x2))
}
