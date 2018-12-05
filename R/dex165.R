#' @title probability mass function for exersize 1.6.5
#'
#' @description a pmf for choosing 5 out of 100 units to test for acceptability
#'
#' @examples pmf_matrix <- dex165()
#'
#' @return a matrix with the value of x and its pmf
#' @return x the x value used to determine the output for the pmf
#' @return pmf the output of the probability mass function using the corresponding x value
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @details pmf for Exercise 1.6.5 part a
#'
#' @export dex165
#'

dex165 <- function() {
  
  x <- 0:5
  
  pmf <- choose(20, x) * choose(80, 5 - x)/choose(100, 5)
  
  tab <- rbind(x, pmf)
  
  return(tab)
}
