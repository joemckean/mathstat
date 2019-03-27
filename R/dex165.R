#' @title probability mass function for Exersize 1.6.5
#'
#' @description Suppose we have a lot of 100 items, 20 of which are defective.
#' Let X be the number of defective item in a random sample of size 5 drawn
#' without replacement.  This function calulates the pmf of X.
#'
#' @examples pmf_matrix <- dex165()
#'
#' @return a matrix containing the table distribution of X.
#' The first row of the table consists of the range of X and the second row
#' the associated probabilities.
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
