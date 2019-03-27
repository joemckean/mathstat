#' @title emperical alpha
#' @description Obtains the empirical alpha level of the t-test when sampling from
#' a contaminated normal distribution.  See Example 4.8.6 on page 297 of HMC.
#'
#' @param nsims an integer, the number of simulations that are to be run
#'
#' @examples nsims <- 10
#' empalpha_error <- empalphacn(nsims)
#'
#' @return a list with empiricalalpha and error.
#' @return empiricalalpha is a numeric value that is the empirical alpha.
#' @return error is a neumeric value that is the calculated error
#'
#' @seealso \code{\link{rcn}} for  generating random contaminated normal deviates.
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export empalphacn
#'


empalphacn <- function(nsims) {
  # checking arguments
  errors <- makeAssertCollection()
  # argument 1 nsims
  errors$push(has_nonan(nsims, 1))
  errors$push(has_noinf(nsims, 1))
  errors$push(is_integer(nsims, 1))
  reportAssertions(errors)
  errors$push(is_nonzero(nsims, 1))
  errors$push(is_positive(nsims, 1))
  reportAssertions(errors)
  # function start
  sigmac <- 25
  eps <- 0.25
  alpha <- 0.05
  n <- 20
  tc <- qt(1 - alpha, n - 1)
  ic <- 0
  # loop nsims times
  for (i in 1:nsims) {
    samp <- rcn(n, eps, sigmac)
    ttest <- (sqrt(n) * mean(samp))/var(samp)^0.5
    # if ttest is greater than tc increment ic
    if (ttest > tc) {
      ic <- ic + 1
    }
  }
  empalp <- ic/nsims
  err <- 1.96 * sqrt((empalp * (1 - empalp))/nsims)
  return(list(empiricalalpha = empalp, error = err))
}
