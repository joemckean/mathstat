#' @title plot sulfurdioxide
#'
#' @description This function creates a histogram using the sulfurdioxide table
#' from the sulfrurdio.rda.
#'
#' @examples plotsulfur()
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to
#' Mathematical Statistics, 8th Ed. Boston: Pearson.
#'
#' @export plotsulfur
#'
#' @importFrom utils data
#'

plotsulfur <- function(){
    sulfurdiov <- unlist(sulfurdio)
    hist(sulfurdiov, xlab = "Sulfurdioxide", ylab = " ", probability = TRUE, ylim = c(0, .04), cex.main = 1.25)
    lines(density(sulfurdiov))
    y <- dnorm(sulfurdiov, 53.91667, 10.07371)
    lines(y~sulfurdiov, lty=2)
}
data("sulfurdio", envir = environment(plotsulfur))
