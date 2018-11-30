#' @title F-Distribution Table for 0.05, 0.25, and 0.01 Critical Points
#'
#' @description Produces the F-distribution table for the 0.950, 0.975,
#' and 0.990 quantiles of the F-distribution. The first column of the table
#' consists of these three quantiles repeating in this fashion. The second column
#' represents degrees of freedom in the denominator for each of these critical points.
#' The rest of the columns are for the first 11 degrees of freedom in the numerator.
#'
#' @return \code{xmat} is the F-distribution table for the 95th quantile.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @seealso fp1() for the F-distribution table of the 0.95 quantile with
#' the numerator having less than ten degrees of freedom, fp2() for the
#' F-distribution table of the 0.95 quantile with the numerator,
#' having greater than nine degrees of freedom., as well as fp3() and
#' fp4() for the F-distribution table of the 0.99 quantile.
#'
#' @examples
#' ftableb()
#'
#' @export ftableb
ftableb <- function() {

    top <- 1:16

    c1 <- rep(c(0.95, 0.975, 0.99), 16)
    c2 <- rep(1, 3)

    c2s <- c2

    for(j in 2:16) {
        c2 <- c2 + 1
        c2s <- c(c2s, c2)
    }

    is <- length(c1)
    js <- length(top)

    ptabs <- matrix(rep(0, 768), ncol=16)

    for(i in 1:is) {
        for(j in 1:js) {
            ptabs[i, j] <- qf(c1[i], top[j], c2s[i])
        }
    }

    ptabs <- round(100 * ptabs)/ 100

    ftableb <- cbind(c1, c2s, ptabs)

    return(ftableb )
}
