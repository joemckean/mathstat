#' @title pdf of Contaminated Normal
#'
#' @description Computes the pdf of the contaminated normal pdf.
#' See expression (3.4.17) on page 194 of HMC.
#'
#' @param w value in the domain of the pdf.
#'
#' @param eps Proportion of contamination. Usually between 0 and 0.25.
#'
#' @param sigma_c Standard deviation of contaminated part.
#' Should be greater than 1.
#'
#' @return Value of pdf at w for a contaminated normal.
#'
#' @references Hogg, R. McKean, J. Craig, A (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson
#'
#' @examples
#' # Example where variables are set then passed in as parameters.
#' w <- 10
#' eps <- 0.5
#' sigma_c <- 0.25
#' p <- dcn(w, eps, sigma_c)
#'
#' # Example where parameters are passed in as values.
#' p <- dcn(10, 0.5, 0.25)
#'
#' @export dcn

#' @importFrom checkmate makeAssertCollection reportAssertions
#' @import stats
#' @import graphics

dcn <- function(w, eps, sigma_c) {
  
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
  
  dcn <- (1 - eps) * dnorm(w) + eps * dnorm(w/sigma_c)/sigma_c
  
  return(dcn)
}
