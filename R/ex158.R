#' @title piecewise cdf graph
#'
#' @description creates a graph where F(x)=0 when x<-1,
#' F(x)=(x+2)/4 when -1<=x<1, and F(x)=1 where 1<=x.
#'
#' @examples ex158()
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export ex158
#'
#' @details creates the cdf for Exercise 1.5.8.

ex158 <- function() {

	plot(c(0, 1) ~ c(-1.25, 1.25),
	     xlab="x",
	     ylab="y",
	     pch=" ")

	segments(-1.25,
	         0,
	         -1,
	         0)
	segments(1.00,
	         1,
	         1.25,
	         1)
	segments(-1,
	         0.25,
	         1,
	         0.75)

	points(-1, 0.25 ,pch=18)
	points(1, 1, pch=18)
	points(-1, 0, pch=21)
	points(1, 0.75, pch=21)

	title("Exercise 1.5.8")
}
