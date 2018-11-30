#' @title Wilcox Power Simulation
#'
#' @description Iterates over specified number of simulations. At each step,
#' generate independent samples, compute each test statistic, and record whether
#' or not each test rejected. For each test, its empirical power is its number
#' of rejections divided by the number of simulations.
#' See section 10.4.4 on page 605 of the book.
#'
#' @param n1 Sample size 1.
#'
#' @param n2 Sample size 2. This value must be divisible by the number of
#' elements in vector Delta.
#'
#' @param nsims Number of iterations (simulations).
#'
#' @param eps Contamination rate (epsilon).
#'
#' @param vc Standard deviation of contaminated part.
#'
#' @param Delta Vector of shifts in location between models. Sample size 2 (n2)
#' must be divisible by the number of ekements in this vector.
#'
#' @param alpha Level of significance of the test.
#'
#' @return Vector containing empirical power of MWW test and t-test.
#'
#' @references Hogg, R. McKean, J. Craig, A (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson
#'
#' @examples
#' # Example where variables are initialized
#' # then passed into function.
#' n1 <- 30
#' n2 <- 30
#' nsims <- 10000
#' eps <- 0.2
#' vc <- 10
#' Delta <- c(-3, 3, 1)
#' alpha <- 0.25
#' results <- wil2powsim(n1, n2, nsims, eps, vc, Delta, alpha)
#'
#'
#' # Example where values are passed directly
#' # into function, along with a default param
#' # override for the Delta param.
#' results <- wil2powsim(30, 30, 10000, 0.20, 10, c(-3, 3, 1), 0.25)
#'
#' @export wil2powsim
#'
#' @importFrom checkmate makeAssertCollection reportAssertions

wil2powsim <- function(	n1,
						n2,
						nsims,
						eps,
						vc,
						Delta=0,
						alpha=.05){

	# INPUT VALIDATION
	errors <- makeAssertCollection()
	# argument 1: n1
	errors$push(has_nonan(n1, 1))
	errors$push(is_oneelement(n1, 1))
	reportAssertions(errors)

	errors$push(is_numeric(n1, 1))
	errors$push(is_nonzero(n1, 1))
	errors$push(is_positive(n1, 1))
	errors$push(has_noinf(n1, 1))
	reportAssertions(errors)

	# argument 2: n2
	errors$push(has_nonan(n2, 2))
	errors$push(is_oneelement(n2, 2))
	reportAssertions(errors)

	errors$push(is_numeric(n2, 2))
	errors$push(is_nonzero(n2, 2))
	errors$push(is_positive(n2, 2))
	errors$push(has_noinf(n2, 2))
	reportAssertions(errors)

	# argument 3: nsims
	errors$push(has_nonan(nsims, 3))
	errors$push(is_oneelement(nsims, 3))
	reportAssertions(errors)

	errors$push(is_numeric(nsims, 3))
	errors$push(is_nonzero(nsims, 3))
	errors$push(is_positive(nsims, 3))
	errors$push(has_noinf(nsims, 3))
	reportAssertions(errors)

	# argument 4: eps
	errors$push(has_nonan(eps, 4))
	errors$push(is_oneelement(eps, 4))
	reportAssertions(errors)

	errors$push(is_numeric(eps, 4))
	errors$push(is_nonzero(eps, 4))
	errors$push(is_positive(eps, 4))
	errors$push(has_noinf(eps, 4))
	reportAssertions(errors)

	# argument 5: vc
	errors$push(has_nonan(vc, 5))
	errors$push(is_oneelement(vc, 5))
	reportAssertions(errors)

	errors$push(is_numeric(vc, 5))
	errors$push(is_nonzero(vc, 5))
	errors$push(is_positive(vc, 5))
	errors$push(has_noinf(vc, 5))
	reportAssertions(errors)

	# argument 6: Delta
	errors$push(has_nonan(Delta, 6))
	reportAssertions(errors)

	errors$push(is_numeric(Delta, 6))
	#errors$push(is_integer(n2 / length(Delta), 6,
	#					   "argument 6 length must divide argument 2"))
	errors$push(has_noinf(Delta, 6))
	reportAssertions(errors)

	# argument 1: alpha
	errors$push(has_nonan(alpha, 7))
	errors$push(is_oneelement(alpha, 7))
	reportAssertions(errors)

	errors$push(is_numeric(alpha, 7))
	errors$push(is_nonzero(alpha, 7))
	errors$push(is_positive(alpha, 7))
	errors$push(has_noinf(alpha, 7))
	reportAssertions(errors)

	# FUNCTION BEGINS

	# Initialize local variables
	indwil <-0
	indt <- 0

	for(i in 1:nsims){
		x <- rcn(n1, eps, vc)
		y <- rcn(n2, eps, vc) + Delta
		# $p.value is a column in the return from wilcox.test()
		if(wilcox.test(y, x)$p.value <= alpha){
			indwil <- indwil + 1
		}
		# $p.value is a column in the return from t.test()
		if(t.test(y, x, var.equal=TRUE)$p.value <= alpha){
			indt <- indt + 1
		}
	}
	powwil <- sum(indwil) / nsims
	powt <- sum(indt) / nsims
	return(c(powwil, powt))
}
