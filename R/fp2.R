#' @title F-Distribution Table for Upper 0.05 Critical Points
#'
#' @description Produces the F-distribution table for the 0.95 quantile
#' of the F-distribution.
#'
#' @details This table more specifically produces the F-distribution table for
#' when alpha equals 0.05. The initial row indicates the degrees of freedom
#' in the numerator for values greater than 9 and the inital column indicates degrees
#' of freedom in the denominator. See page 712 for more details.
#'
#' @return \code{xmat} is the F-distribution table for the 95th quantile
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @seealso fp1() for the F-distribution table of the 0.95 quantile with
#' the numerator having less than ten degrees of freedom, as well as fp3() and
#' fp4() for the F-distribution table of the 0.99 quantile.
#'
#' @examples
#' fp2()
#'
#' @export fp2

fp2 <- function() {

    numdf <- c(10,
               15,
               20,
               25,
               30,
               40,
               60,
               120)

    denomdf <- c(1:30,
                 40,
                 60,
                 120)

    ic <- length(denomdf)

    xmat <- matrix(rep(0, 8 * ic), ncol=8)

    for(i in 1:ic) {
          for(j in 1:8) {
             xmat[i, j] <- round(100 * qf(0.95, numdf[j], denomdf[i]))/ 100
          }
    }

    xmat <- cbind(denomdf, xmat)
    xmat <- rbind(c(0, numdf), xmat)

    vec <- c(0, round(100 * qchisq(0.95, numdf)/ numdf)/ 100)

    xmat<- rbind(xmat, vec)

    vec2 <- c(0, round(100 * denomdf/ qchisq(0.05, denomdf))/ 100, 1)

    xmat <- cbind(xmat, vec2)

    return(xmat)
}
