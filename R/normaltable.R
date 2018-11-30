#' @title  Normal Distribution Table
#'
#' @description Generates the normal probabilities for values of \emph{z} that is
#' greater than or equal to zero. See page 709 for more details.
#'
#' @return \code{pz} is the normal distribution table for values of \emph{z} that is
#' greater than or equal to zero.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' normaltable()
#'
#' @export normaltable

normaltable <-function() {

    za <- seq(0.00, 3.59, 0.01)

    pz <- t(matrix(round(pnorm(za),digits=4),nrow=10))

    colnames(pz) <- seq(0, 0.09, 0.01)
    rownames(pz) <- seq(0.0, 3.5, 0.1)

    return(pz)
}
