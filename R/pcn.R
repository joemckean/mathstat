#' @title Probability of Contaminated Normal (CDF)
#'
#' @description Computes the CDF of the contaminated normals, represented as
#' \eqn{P(W \le w)} where W is the random variable of interest.
#' See expression 3.4.19 on page 194 of the book.
#'
#' @param w Variable bound on random variable of interest W.
#'
#' @param eps Proportion of contamination. Usually between 0 and 0.25.
#'
#' @param sigma_c Standard deviation of contaminated part.
#' Should be greater than 1.
#'
#' @return Numerical representation of probability of contaminated normals
#' \eqn{P(W \le w)}
#'
#' @references Hogg, R. McKean, J. Craig, A (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson
#'
#' @examples
#' # Example where variables are set then passed in as parameters.
#' w <- 10
#' eps <- 0.5
#' sigma_c <- 0.25
#' p <- pcn(w, eps, sigma_c)
#'
#' # Example where parameters are passed in as values.
#' p <- pcn(10, 0.5, 0.25)
#'
#' @export pcn

pcn <- function(w, eps, sigma_c) {
  
  # INPUT VALIDATION
  errors <- makeAssertCollection()
  # argument 1: w
  errors$push(has_nonan(w, 1))
  errors$push(is_oneelement(w, 1))
  reportAssertions(errors)
  
  errors$push(is_numeric(w, 1))
  errors$push(has_noinf(w, 1))
  reportAssertions(errors)
  
  # argument 2: eps
  errors$push(has_nonan(eps, 2))
  errors$push(is_oneelement(eps, 2))
  reportAssertions(errors)
  
  errors$push(is_numeric(eps, 2))
  errors$push(has_noinf(eps, 2))
  reportAssertions(errors)
  
  # argument 3: sigma_c
  errors$push(has_nonan(sigma_c, 3))
  errors$push(is_oneelement(sigma_c, 3))
  reportAssertions(errors)
  
  errors$push(is_numeric(sigma_c, 3))
  errors$push(has_noinf(sigma_c, 3))
  errors$push(is_positive(sigma_c, 3))
  errors$push(is_nonzero(sigma_c, 3))
  reportAssertions(errors)
  
  # FUNCTION BEGINS
  
  pcn <- ((1 - eps) * pnorm(w)) + (eps * pnorm(w/sigma_c))
  
  return(pcn)
}
