#' @title The First Iteration of the EM Step
#'
#' @description This function returns one iteration of the EM
#' step for excersize 6.6.8. The initial estimate for the step
#' is theinput vector theta0 while the ouput are the onestep
#' esimates as given on page 409 of the text.
#'
#' @param x a vector of quantiles.
#' @param theta0 a vector, first 2 elements are means, next 2
#' elements are standard deviations, and the last element is
#' the perportion of data from the first population
#'
#' @examples x <- c(5, 7, 2, 1,9)
#' theta0 <- c(.2, .6, .8, .9, .95)
#' mixnormal(x, theta0)
#'
#' @return returns a vector, with mu1, mu2, sig1, sig2 and p
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export mixnormal


mixnormal <- function(x, theta0) {
  # checking arguments
	errors <- makeAssertCollection()
  # checking arguments argument 1 x
  errors$push(is_numvector(x, 1))
  errors$push(has_nonan(x, 1))
  errors$push(has_noinf(x, 1))
  # argument 2 theta0
  errors$push(has_elements(theta0, 2, 5))
  errors$push(has_noinf(theta0, 2))
  errors$push(has_nonan(theta0, 2))
  errors$push(is_numvector(theta0, 2))
  reportAssertions(errors)
  # theta0[3]
  errors$push(is_nonzero(theta0[3], 2,
              "element 3 in argument 2 cannot be zero"))
  errors$push(is_positive(theta0[3], 2,
              "element 3 in argument 2 must be positive"))
  # theta0[4]
  errors$push(is_nonzero(theta0[4], 2,
              "element 4 in argument 2 cannot be zero"))
  errors$push(is_positive(theta0[4], 2,
              "element 4 in argument 2 must be positive"))
  # theta0[5]
  errors$push(is_xrange(theta0[5], 2, 0, 1,
              "element 5 in argument 2 must be greater than 0 and less than 1"))
  reportAssertions(errors)
  # function starts
  # calculate gamma
  part1 <- (1 - theta0[5]) * dnorm(x, theta0[1], theta0[3])
  part2 <- theta0[5] * dnorm(x, theta0[2], theta0[4])
  gam <- part2/(part1 + part2)
  # calculate denomination for sig1 and sig2
  denom1 <- sum(1 - gam)
  denom2 <- sum(gam)
  # calculate mu1 and mu2
  mu1 <- sum((1 - gam) * x)/denom1
  mu2 <- sum(gam * x)/denom2
  # calculate sig1 and sig2
  sig1 <- sqrt(sum((1 - gam) * ((x - mu1)^2))/denom1)
  sig2 <- sqrt(sum(gam * ((x - mu2)^2))/denom2)
  # calculate the p value
  p <- mean(gam)

  mixnormal <- c(mu1, mu2, sig1, sig2, p)

  return(mixnormal)
}
