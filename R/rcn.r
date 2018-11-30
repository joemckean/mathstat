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

rcn <- function(n = 0,
                eps = 0.0,
                sigma_c = 1.0) {

    # Convert input to numeric
    n <- as.numeric(n)

    # Error Handling

    if (is.na(n)) {
        stop(gettext("input object 'n' is non-numeric"))

    } else if (n < 0.0) {
        stop(gettext("'n' cannot be less than zero"))

    } else if (n > 10000e4) {
        stop(gettext("'n' cannot be greater than 10000e4"))

    } else if (identical(n, 0.0)) {
        warning(gettext("'n' cannot equal zero"))

    }

	ind <- rbinom(n, 1, eps) # random generation for the binomial distribution
	x <- rnorm(n) # random generation for the normal distribution
	rcn <- x * (1 - ind) + sigma_c * x * ind

	return(rcn)
}
