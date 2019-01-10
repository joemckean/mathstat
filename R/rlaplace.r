#' @title Random Laplace Deviates
#'
#' @description  Generates variates from a standard Laplace distribution, which is discussed on page
#' 77 of the text (HMC).   
#'
#' @param n Size of random sample to generate
#' @details Random variates are generated from a standard Laplace distribution.
#'
#' @return n x 1 numeric vector containing the random deviates.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' rlaplace(n=1000)
#'
#' @export rlaplace

rlaplace <- function(n = 0) {
  
  # checking arguments
#J  errors <- makeAssertCollection()
  # argument 1 n
#J  errors$push(is_oneelement(n, 1))
#J  errors$push(is_numeric(n, 1))
#J  errors$push(has_nonan(n, 1))
#J  errors$push(has_noinf(n, 1))
#J  errors$push(is_nonzero(n, 1))
#J  errors$push(is_positive(n, 1))
  # argument check results
#J  reportAssertions(errors)
  

   x = rexp(n)
   ind = 2*(rbinom(n,1,.5)-.5)
   rlaplace = x*ind
   return(rlaplace)
}
