#' @title Hierarchal Bayes Model
#'
#' @description Computes the Gibbs sampler for the two conditional pdf's,
#' g(lambda | x, b) and g(b | x, lambda).
#'
#' @param nsims Number of iterations for the Gibbs sampler
#' @param x Value observed
#' @param tau Statistic used to measure the ordinal association between
#' @param kstart Indicates at which value of nsims the Gibbs sample commences
#'
#' @details Computes the solution to Example 11.4.2 on page 681.
#'
#' @return clambda contains the whole simulated sequence (nsims + kstart)
#' of Lambdas and gibbslamda contains the ater burn part (kstart+1: nsims + kstart).
#' Likewise, cb and gibbsb for the sequence of B's.
#'
#' @seealso \code{\link{gibbser2}}
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to Mathematical
#' Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' hierarch1(nsims = 300, x = 6, tau = 0.05, kstart = 20)
#' hierarch1(nsims = 150, x = 3, tau = 0.045, kstart = 35)
#'
#' @export hierarch1

hierarch1 <- function(nsims = 0, x = 0, tau = 0.05, kstart = 1) {
  # checking arguments
  errors <- makeAssertCollection()
  # argument 1 nsims
  errors$push(has_nonan(nsims, 1))
  errors$push(has_noinf(nsims, 1))
  errors$push(is_nonzero(nsims, 1))
  errors$push(is_positive(nsims, 1))
  errors$push(is_numeric(nsims, 1))
  errors$push(is_oneelement(nsims, 1))
  # argument 2 x
  errors$push(has_nonan(x, 2))
  errors$push(has_noinf(x, 2))
  errors$push(is_nonzero(x, 2))
  errors$push(is_positive(x, 2))
  errors$push(is_numeric(x, 2))
  errors$push(is_oneelement(x, 2))
  # argument 3 tau
  errors$push(has_nonan(tau, 3))
  errors$push(has_noinf(tau, 3))
  errors$push(is_nonzero(tau, 3))
  errors$push(is_positive(tau, 3))
  errors$push(is_numeric(tau, 3))
  errors$push(is_oneelement(tau, 3))
  # argument 4 kstart
  errors$push(has_nonan(kstart, 4))
  errors$push(has_noinf(kstart, 4))
  errors$push(is_nonzero(kstart, 4))
  errors$push(is_positive(kstart, 4))
  errors$push(is_numeric(kstart, 4))
  errors$push(is_oneelement(kstart, 4))
  # argument check results
  reportAssertions(errors)
  # Function starting point
  bold <- 1

  clambda <- rep(0, (nsims + kstart))
  cb <- rep(0, (nsims + kstart))

  for (i in 1:(nsims + kstart)) {
    clambda[i] <- rgamma(1, shape = (x + 1), scale = (bold/(bold +
      1)))
    newy <- rgamma(1, shape = 2, scale = (tau/(clambda[i] * tau +
      1)))

    cb[i] <- 1/newy
    bold <- 1/newy
  }

  gibbslambda <- clambda[(kstart + 1):(nsims + kstart)]
  gibbsb <- cb[(kstart + 1):(nsims + kstart)]

  return(list(clambda = clambda, cb = cb, gibbslambda = gibbslambda,
    gibbsb = gibbsb))
}
