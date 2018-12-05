#' @title  t-Distribution Table
#'
#' @description Generates the t-distribution probabilities for the selected quantiles
#' and degrees of freedom.
#'
#' @details This table more specifically produces the t-distribution table
#' for the \emph{0.900}, \emph{0.950}, \emph{0.975}, \emph{0.990}, \emph{0.995},
#' \emph{0.999} quantiles. The initial row indicates the selected quantiles and
#' the inital column indicates 'r' degrees of freedom See page 710 for more details.
#'
#' @return \code{tab} is the t-distribution table.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' ttable()
#'
#' @export ttable

ttable <- function() {
  ps <- c(0.9, 0.925, 0.95, 0.975, 0.99, 0.995, 0.999)
  df <- 1:30
  tab <- c()
  for (r in df) {
    tab <- rbind(tab, qt(ps, r))
  }
  df <- c(df, Inf)
  nq <- qnorm(ps)
  tab <- rbind(tab, nq)
  tab <- cbind(df, tab)
  return(tab)
}
