#' @title Birthday Solver
#'
#' @description Calcuates the probability of at least two out of 'n' people
#' in a room having the same birthday, assuming that birthdays are equilikely
#' throughout the year.
#'
#' @param n Sample size of the number of people in a room
#'
#' @details Computes the solution to Example 1.3.3 (8th Edition)
#'
#' @return The probability \code{bday} of at least two people having the same
#' birthdays
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to Mathematical
#' Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' bday(2)
#' bday(10)
#'
#' @export bday

bday <- function(n = 0) {
  # Default value for number of people in a sample size is zero
  
  # checking arguments
  errors <- makeAssertCollection()
  # argument 1 n
  errors$push(has_nonan(n, 1))
  errors$push(has_noinf(n, 1))
  errors$push(is_nonzero(n, 1))
  errors$push(is_positive(n, 1))
  errors$push(is_numeric(n, 1))
  errors$push(is_oneelement(n, 1))
  # argument check results
  reportAssertions(errors)
  
  # Error Handling input value 'n' 365 represents the number of days
  # in a year
  if (n > 365) {
    stop(gettext("'n' cannot be larger than 365"))
    
  } else {
    
    bday <- 1
    
    nm1 <- n - 1
    
    for (j in 1:nm1) {
      bday <- bday * ((365 - j)/365)
      
    }
    
    bday <- 1 - bday
    
    return(bday)
    
  }
  
}

