#' @title Random Contaminated Normal Deviates
#'
#' @description  Generates data sets from a contaminated normal distribution as
#'               in Example 4.9.2
#'
#' @param n Size of random sample to generate
#' @param eps Proportion of contamination
#' @param sigma_c Standard deviation of contaminated component
#'
#' @details With probability (1-eps) a deviates are drawn from a standard normal
#'          distribution. With probability eps deviates are drawn from a normal
#'          distribution with mean 0 and standard devation sigma_c.
#'
#' @return n x 1 numeric vector containing the random deviates.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' rcn(n = 12, eps = 0.20, sigma_c = 4)
#' rcn(n = 30, eps = 0.30, sigma_c = 6)
#'
#' @export rcn

rcn <- function(n = 0, eps = 1e-04, sigma_c = 1) {
  
  # checking arguments
  errors <- makeAssertCollection()
  # argument 1 n
  errors$push(is_oneelement(n, 1))
  errors$push(is_numeric(n, 1))
  errors$push(has_nonan(n, 1))
  errors$push(has_noinf(n, 1))
  errors$push(is_nonzero(n, 1))
  errors$push(is_positive(n, 1))
  # argument 2 eps
  errors$push(has_nonan(eps, 2))
  errors$push(has_noinf(eps, 2))
  errors$push(is_numeric(eps, 2))
  errors$push(is_oneelement(eps, 2))
  # argument 2 sigma_c
  errors$push(has_nonan(sigma_c, 3))
  errors$push(has_noinf(sigma_c, 3))
  errors$push(is_positive(sigma_c, 3))
  errors$push(is_numeric(sigma_c, 3))
  errors$push(is_oneelement(sigma_c, 3))
  # argument check results
  reportAssertions(errors)
  
  # Edge case check for input argument 'eps'
  if (eps < 0 || eps >= 1) {
    stop(gettext("input argument 'eps' must be between zero and one"))
  }
  
  
  ind <- rbinom(n, 1, eps)  # random generation for the binomial distribution
  x <- rnorm(n)  # random generation for the normal distribution
  rcn <- x * (1 - ind) + sigma_c * x * ind
  
  return(rcn)
}
