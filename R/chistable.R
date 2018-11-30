#' @title Chi-Square Distribution
#'
#' @description Presents selected quantiles of the chi-square distribution.
#'
#' @details This table more specifically produces the chi-square distribution table
#' for the \emph{0.01}, \emph{0.025}, \emph{0.050}, \emph{0.100}, \emph{0.900}, \emph{0.950},
#' \emph{0.975}, and \emph{0.990} quantiles. The initial row indicates the selected
#' quantiles and the inital column indicates 'r' degrees of freedom See page 708
#' for more details.
#'
#' @return \code{chistable} is the chi-square distribution table for the selected
#' quantiles.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' chistable()
#'
#' @export chistable

chistable <- function() {

    x <- c(0.01,
           0.025,
           0.50,
           0.10,
           0.90,
           0.95,
           0.975,
           0.99)

    top <- 30

    ptabs <- matrix(rep(0, 240), ncol=8)

    for(i in 1:length(x)) {
        ptabs[, i] <- qchisq(x[i], 1:top)
    }

    ptabs <- round(1000 * ptabs)/ 1000
    chistable <- cbind(1:top, ptabs)

    return(chistable)

}
