#' @title Population Quantile Plots
#'
#' @description Function which obtains the normal, Laplace, and
#' exponential quantiles as shown in Figure 4.4.1.
#' See Example 4.4.6 on pages 260 and 261 of the book.
#'
#' @param vec Vector containing data for graph.
#'
#' @references Hogg, R. McKean, J. Craig, A (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson
#'
#' @examples
#' # Example where parameters are passed in as variables.
#' vec <- 1:1000
#' qqplotc4s2(vec)
#'
#' # Example where parameters are passed in as values.
#' qqplotc4s2(1:1000)
#'
#' @export qqplotc4s2

qqplotc4s2 <- function(vec) {
  
  # INPUT VALIDATION
  errors <- makeAssertCollection()
  # argument 1: vec
  errors$push(has_nonan(vec, 1))
  reportAssertions(errors)
  
  errors$push(is_numeric(vec, 1))
  errors$push(has_noinf(vec, 1))
  reportAssertions(errors)
  
  # FUNCTION BEGINS
  
  n <- length(vec)
  ps <- (1:n)/(n + 1)
  normalqs <- qnorm(ps)
  y <- sort(vec)
  par(mfrow = c(2, 2))
  boxplot(y, ylab = "x")
  title(main = "Panel A")
  plot(normalqs, y, xlab = "Normal quantiles", ylab = "Sample quantiles")
  title(main = "Panel B", xlab = "Normal quantiles", ylab = "Sample quantiles")
  plot(qlaplace(ps), y, xlab = "Laplace quantiles", ylab = "Sample quantiles")
  title(main = "Panel C")
  plot(qexp(ps), y, xlab = "Exponential quantiles", ylab = "Sample quantiles")
  title(main = "Panel D")
}
