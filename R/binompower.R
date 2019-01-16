# The values below sketch power functions for tests described in
# Example 4.5.2.  But it is easy to change these for another
# binomial situation

#' @title Power Functions for Binomial Tests
#'
#' @description Plots the power functions for binomial tests which validate
#' in general the monotonicity of the power function for binomial tests.
#'
#' @details Produces a graph similar to what is seen in Figure 4.5.1 (8th Edition).
#' The power functions in the plot are described in Example 4.5.2 (8th Edition).
#'
#' @return A plot of the power functions for binomial tests of size alpha
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to Mathematical
#' Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' binompower()
#'
#' @export binompower

binompower <- function() {
  
  n <- 20
  
  k1 <- 11  # Reject if S <= k1
  k2 <- 12  # Reject if S <= k2
  
  x <- seq(0.4, 1, 0.01)  # Input arguments must be doubles
  
  pow1 <- pbinom(k1, n, x)
  pow2 <- pbinom(k2, n, x)
  
  # Note: Margin solution came from this source:
  # https://stackoverflow.com/questions/23050928/error-in-plot-new-figure-margins-too-large-scatter-plot
  par(mar = c(1, 1, 1, 1))
  
  
  plot(x, pow2, xlab = "p", ylab = expression(gamma(p)), ylim = c(0, 
    1), xlim = c(0.35, 1), type = "l", lty = 2)
  
  lines(x, pow1, lty = 1)
  title("Power Functions for Binomial Tests")
  text(0.72, 0.4, "Level 0.23")
  text(0.54, 0.4, "Level 0.11")
}
