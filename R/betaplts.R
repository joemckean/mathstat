#' @title Beta Distribution Plots
#'
#' @description this function plots the beta pdf for alpha equaling 2,
#' 3, 4, and 5 and beta equaling 2, 3, 4, and 5.   See Example 3.3.5 of HMC.
#'
#' @examples betaplts()
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export betaplts
#'
#' @import graphics
#'

betaplts <- function() {
  
  par(mfrow = c(4, 4))
  
  r1 <- 2:5
  r2 <- 2:5
  
  x <- seq(0.01, 0.99, 0.01)
  
  for (a in r1) {
    for (b in r2) {
      plot(dbeta(x, a, b) ~ x)
      title(paste("alpha = ", a, "beta = ", b))
    }
  }
  
}
