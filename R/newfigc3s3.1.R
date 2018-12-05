#' @title Figure 3.3.1
#'
#' @description This function produces several gamma distributions with various
#' inputs for the parameters \emph{alpha} and \emph{beta}. The top plot illustrates
#' how changing values for \emph{alpha} influences the gamma distribution with
#' \emph{beta} set to equal 4. The plot below illustrates how changing values for
#' \emph{beta} influences the gamma distribution with \emph{alpha} set to to equal
#' 4.
#'
#' @details The images and descriptions of these graphs can also be found on pages
#' 175 and 176 in the book.
#'
#' @return Two gamma distribution plots with various inputs for paramaters
#' \emph{alpha} and \emph{beta}.
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to Mathematical
#' Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' newfigc3s3.1()
#'
#' @export newfigc3s3.1

newfigc3s3.1 <- function() {
  
  dumx <- runif(100, 0, 15)
  dumy <- runif(100, 0, 0.3)
  
  # Note: Margin solution came from this source:
  # https://stackoverflow.com/questions/23050928/error-in-plot-new-figure-margins-too-large-scatter-plot
  par(mfrow = c(2, 1), mar = c(1, 1, 1, 1))
  
  # Upper Plot
  plot(dumx, dumy, axes = TRUE, xlim = c(0, 15), ylim = c(0, 0.3), pch = " ", xlab = expression(x), 
    ylab = expression(f(x)))
  
  x <- seq(0, 34, 0.1)
  
  y <- dgamma(x, shape = 0.25, scale = 4)
  
  lines(x, y)
  
  text(2.3, 0.25, expression(paste(alpha, " = 0.25")), cex = 2)
  
  y <- dgamma(x, shape = 0.5, scale = 4)
  
  lines(x, y)
  
  text(1, 0.04, expression(paste(alpha, " = 0.5")), cex = 2)
  
  y <- dgamma(x, shape = 1, scale = 4)
  
  lines(x, y)
  
  text(6, 0.1, expression(paste(alpha, " = 1")), cex = 2)
  
  title(main = expression(paste(beta, " = 4")), cex = 2)
  
  # Lower Plot
  dumx <- runif(100, 0, 34)
  dumy <- runif(100, 0, 0.13)
  
  plot(dumx, dumy, axes = TRUE, xlim = c(0, 34), ylim = c(0, 0.12), pch = " ", xlab = "x", ylab = "f(x)")
  
  title(main = expression(paste(alpha, " = 4")), cex = 2)
  
  y <- dgamma(x, shape = 4, scale = 2)
  
  lines(x, y)
  
  text(10, 0.1, expression(paste(beta, " = 2")), cex = 2)
  
  y <- dgamma(x, shape = 4, scale = 3)
  
  lines(x, y)
  
  text(14, 0.07, expression(paste(beta, " = 3")), cex = 2)
  
  y <- dgamma(x, shape = 4, scale = 4)
  
  lines(x, y)
  
  text(22, 0.04, expression(paste(beta, " = 4")), cex = 2)
}
