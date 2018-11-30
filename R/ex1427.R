#' @title Number drawing game
#'
#' @description in this game first a number is drawn from a set 1-20, let x
#' be the number drawn. Next another number is drawn from a set x-25 if the
#' second number is greater than 21 then its a win.
#'
#' @examples result <- ex1427()
#'
#' @return returns 1 if the player wins; else returns 0
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export ex1427
#'
#' @details game simulation for Exercise 1.4.27 on page 36
#'

ex1427 <- function() {
	# function starts
	pwin <- 0
	x <- sample(1:20, 1)
	y <- sample(x:25, 1)
  # if y is greater than 21 you win/assign 1 to pwin
	if(y > 21) {
		pwin <- 1
	}
  # pwin 1 if you win 0 if you lose
	return(pwin)
}
