#' @title Get Confidence Intervals
#'
#' @description  Calculates t confidence intervals for m samples each of size n
#' contained in the R matrix mat which is a \emph{m x n} matrix. Each row is a
#' sample of size n. The function is discussed in Exercise 4.2.11 on page 245 of
#' HMC (2018).
#'
#' @param mat an m x n matrix
#' @param cc confidence coefficient, or the confidence level of the intervals
#'
#' @details This function is used to calculate t confidence intervals for m
#' samples each of size n contained in the R matrix mat, which is a \emph{m x n}
#' matrix. Each row is a sample of size n.
#'
#' @return (1 - alpha)100\% confidence intervals in a m x 2 matrix
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' n <- 10
#' m <- 50
#' mat <- matrix(rnorm(m * n), ncol = n)
#' getcis(mat = mat, cc = 0.95)
#' getcis(mat = mat, cc = 0.90)
#'
#' @export getcis

getcis <- function(mat, cc = 0.9) {
  
  # checking arguments
  errors <- makeAssertCollection()
  # argument 1 mat
  errors$push(is_matrix(mat, 1))
  errors$push(is_numeric(mat, 1))
  errors$push(is_manyelement(mat, 1))
  errors$push(has_nonan(mat, 1))
  errors$push(has_noinf(mat, 1))
  # argument 2 cc
  errors$push(is_numeric(cc, 2))
  errors$push(has_nonan(cc, 2))
  errors$push(has_noinf(cc, 2))
  errors$push(is_inrange(cc, 2, 0, 1))
  # argument check results
  reportAssertions(errors)
  
  # Function start point
  numb <- nrow(mat)
  ci <- c()
  
  for (j in 1:numb) {
    ci <- rbind(ci, t.test(mat[j, ], conf.level = cc)$conf.int)
  }
  
  return(ci)
}
