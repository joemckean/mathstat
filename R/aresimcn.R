#' @title A Relative Efficency Investigation Simulation
#'
#' @description Calculates the relative efficency between estimators for finite
#' samples sizes. The investigation involves making comparisons over families
#' of distributions, as well as a selection of sample sizes.
#'
#' @param n Size of generated samples
#' @param nsims Number of simulations
#' @param eps Proportion of contamination
#' @param vc Standard deviation of contaminated component
#'
#' @details An illustration of this function can be found in Section 10.3.4 on
#' page 596. This example uses samples of size 30 and runs 10,000 simulations.
#'
#' @seealso mses() for calculating mean square error and rcn() for generating
#' random contaminated normal deviates.
#'
#' @return \code{aresimcn} is an estimate of relative efficency.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' aresimcn(30, 1000, 0.25, 3)
#' aresimcn(30, 100, 0.20, 25)
#'
#' @export aresimcn

aresimcn <- function(n = 0, nsims = 1, eps = 0, vc = 1) {
  # checking arguments
  errors <- makeAssertCollection()
  # argument 1 n
  errors$push(is_oneelement(n, 1))
  errors$push(is_numeric(n, 1))
  errors$push(has_nonan(n, 1))
  errors$push(has_noinf(n, 1))
  errors$push(is_nonzero(n, 1))
  errors$push(is_positive(n, 1))
  # argument 2 nsims
  errors$push(is_oneelement(nsims, 2))
  errors$push(is_numeric(nsims, 2))
  errors$push(has_nonan(nsims, 2))
  errors$push(has_noinf(nsims, 2))
  errors$push(is_nonzero(nsims, 2))
  errors$push(is_positive(nsims, 2))
  # argument 3 eps
  errors$push(has_nonan(eps, 3))
  errors$push(has_noinf(eps, 3))
  errors$push(is_numeric(eps, 3))
  errors$push(is_oneelement(eps, 3))
  # argument 4 vc
  errors$push(has_nonan(vc, 4))
  errors$push(has_noinf(vc, 4))
  errors$push(is_positive(vc, 4))
  errors$push(is_numeric(vc, 4))
  errors$push(is_oneelement(vc, 4))
  # argument check results
  reportAssertions(errors)
  # Edge case check for input argument 'eps'
  if (eps < 0 || eps >= 1) {
    stop(gettext("input argument 'eps' must be between zero and one"))
  }
  # Function starting postion
  chl <- c()
  cxbar <- c()
  
  for (i in 1:nsims) {
    x <- rcn(n, eps, vc)
    chl <- c(chl, wilcox.test(x, conf.int = TRUE)$est)
    cxbar <- c(cxbar, t.test(x, conf.int = TRUE)$est)
  }
  
  aresimcn <- mses(cxbar, 0)/mses(chl, 0)
  
  return(aresimcn)
}
