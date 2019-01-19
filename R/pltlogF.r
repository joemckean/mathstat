#' @title Plot Log Function
#'
#' @description  This function plots the pdf of the logF distribution.
#' See Figure 1.7.2 on page 54 of HMC.
#'
#' @examples pltlogF()
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export pltlogF
#'

pltlogF <- function() {
  x <- seq(-8, 22, 0.01)
  y <- dlogF(x)
  
  plot(x, y, axes = FALSE, xlab = " ", ylab = " ", type = "l")
  
  q1 <- qlogF(0.25)
  q2 <- qlogF(0.5)
  q3 <- qlogF(0.75)
  d1 <- dlogF(q1)
  d2 <- dlogF(q2)
  d3 <- dlogF(q3)
  
  axis(2, at = c(0, d2, d3), labels = c(" ", "0.10", "0.05"), pos = -8)
  axis(1, at = c(-8, q1, q2, q3), labels = c("-8", expression(italic(bold(q[1]))), 
    expression(italic(bold(q[2]))), expression(italic(bold(q[3])))), 
    pos = 0, cex = 2)
  segments(q1, 0, q1, d1, cex = 2)
  segments(q2, 0, q2, d2, cex = 2)
  segments(q3, 0, q3, d3, cex = 2)
  arrows(q3, 0, 22, 0)
  arrows(-8, d2, -8, 0.113)
  text(23, -5e-04, expression(italic(x)), cex = 1.2)
  text(-8, 0.116, expression(italic(f(x))), cex = 1.2)
}
