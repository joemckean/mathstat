#' @title F-Distribution Table for Upper 0.01 Critical Points
#'
#' @description Produces the F-distribution table for the 0.99 quantile
#' of the F-distribution.
#'
#' @details This table more specifically produces the F-distribution table for
#' when alpha equals 0.01. The initial row indicates the first nine degrees
#' of freedom in the numerator and the inital column indicates degrees of freedom in
#' the denominator. See page 713 for more details.
#'
#' @return \code{xmat} is the F-distribution table for the 99th quantile
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @seealso fp4() for the F-distribution table of the 0.99 quantilss with
#' the numerator having greater than nine degrees of freedom, as well as fp1() and
#' fp2() for the F-distribution table of the 0.95 quantile.
#'
#' @examples
#' fp3()
#'
#' @export fp3

fp3 <- function() {

    numdf <- 1:9
    denomdf <- c(1:30,
                 40,
                 60,
                 120)

    ic <- length(denomdf)

    xmat <-  matrix(rep(0, 9 * ic), ncol=9)

    for(i in 1:ic) {
          for(j in 1:9) {
             xmat[i, j] <- round(100 * qf(0.99, j, denomdf[i]))/ 100
          }
    }

    xmat <- cbind(denomdf, xmat)
    xmat <- rbind(c(0, numdf), xmat)

    vec <- c(0, round(100 * qchisq(0.99, numdf)/ numdf)/ 100)

    xmat <- rbind(xmat, vec)

    return(xmat)
}
