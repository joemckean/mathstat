#' @title Plot Ellipse
#'
#' @description this function plots an ellipse of varying size and coridnates of
#' the center
#'
#' @param p floating point number, between 0 and 1 adjusts the size of the ellipse
#' @param b a 2x2 matrix whith a determinate grater than 0 and all elements are positive, adjusts the shape of the ellipse
#' @param mu a vector of length 2, adjusts the center of the ellipse
#'
#'
#' @examples p <- .85
#' b <- matrix(c(2,1,1,3), nrow=2)
#' mu <- c(3, 3)
#' ellipmake(p, b, mu)
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export ellipmake
#'

# Part of this code was obtained from an annonymous author at the site
# http://stats.stackexchange.com/questions/9898/

ellipmake <- function(p = 0.95, b = matrix(c(1, 0.75, 0.75, 1), nrow = 2), 
  mu = c(5, 2)) {
  # checking arguments
  errors <- makeAssertCollection()
  # first argument p
  errors$push(has_nonan(p, 1))
  errors$push(is_numeric(p, 1))
  reportAssertions(errors)
  errors$push(is_xrange(p, 1, 0, 1))
  # second argument b
  errors$push(has_nonan(b, 2))
  errors$push(has_noinf(b, 2))
  errors$push(is_numeric(b, 2))
  reportAssertions(errors)
  errors$push(is_posdetmat2(b, 2))
  errors$push(is_posmatrix2(b, 2))
  # third argument mu
  errors$push(has_nonan(mu, 3))
  errors$push(has_noinf(mu, 3))
  errors$push(is_numvector(mu, 3))
  errors$push(has_elements(mu, 3, 2))
  reportAssertions(errors)
  # function starts
  csq <- qchisq(p, 2)
  B <- csq * b
  A <- solve(B)
  eig <- eigen(A)
  gam <- eig$vectors
  lam2 <- sqrt(diag(eig$values))
  theta <- seq(0, 2 * pi, length.out = 200)
  y1 <- cos(theta)
  y2 <- sin(theta)
  ym <- cbind(y1, y2)
  xm <- ym %*% solve(lam2) %*% t(gam)
  xm[, 1] <- xm[, 1] + mu[1]
  xm[, 2] <- xm[, 2] + mu[2]
  # create plot
  plot(xm[, 2] ~ xm[, 1], pch = " ")
  lines(xm[, 2] ~ xm[, 1])
  title(main = paste("Chi-sq Probability ", p))
}
