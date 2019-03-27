#' @title Laplace PDF
#'
#' @description The function dlaplace returns the value of the pdf of
#' the standard Laplace distribution, which is discussed on page
#' 77 of the text (HMC).   
#'
#' @param x a vector of floating point numbers between -infty and +infinity.
#'
#' @return returns the corresponding vector of values of the pdf
#' for the standard laplace distribution. 
#'
#' @examples x <- seq(-5,5,.1); y<- dlaplace(x); plot(y~x)
#' ##  Obtains a plot of the pdf.
#' ##
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @details the pdf is:\cr
#' f(x) =  (1/2)*exp(-|x|)\cr   -Inf < x < Inf\cr
#'
#' @export dlaplace
#'

dlaplace <- function(x) {
  # checking argument
  errors <- makeAssertCollection()
  # argument 1 x
  # INPUT VALIDATION
  errors <- makeAssertCollection()
  # argument 1: x
  errors$push(has_nonan(x, 1))
  reportAssertions(errors)

  errors$push(is_numeric(x, 1))
  errors$push(has_noinf(x, 1))
  reportAssertions(errors)
  # function starts
  dlaplace <- (1/2)*exp(-abs(x))
  return(dlaplace)
}
