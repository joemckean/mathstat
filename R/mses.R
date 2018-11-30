#' @title Mean Squared Error
#'
#' @description As part of the Monte Carlo investigation of the relative efficency
#' between estimators for finite sample sizes, this function calculates the sum
#' of squared differences of the inputted sample observations against the
#' value of \code{theta0}. The sum is divided by size of the sample \emph{n}.
#'
#' @param x A numeric vector containing sample size \emph{n}
#' @param theta0 Variance of sample \emph{x}
#'
#' @details The Monte Carlo investigation of relative efficency between estimators
#' for finite sample sizes is covered in Section 10.3.4 on page 595.
#'
#' @return The mean squared error of the inputted sample and value for theta.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' sample1 <- rcn(n = 30, eps = 0.20, sigma_c = 25)
#' sample2 <- rcn(n = 20, eps = 0.20, sigma_c = 25)
#'
#' theta1 <- var(sample1)
#' theta2 <- var(sample2)
#'
#' MSE1 <- mses(sample1, theta1)
#' MSE2 <- mses(sample2, theta2)
#'
#' @export mses

mses <- function(x,
                 theta0 = 0) {
    # checking arguments
    errors <- makeAssertCollection()
    # argument 1 x
    errors$push(is_manyelement(x, 1))
    errors$push(is_numeric(x, 1))
    errors$push(is_numvector(x, 1))
    errors$push(has_nonan(x, 1))
    errors$push(has_noinf(x, 1))
    # argument 2 theta0
    errors$push(is_oneelement(theta0, 2))
    errors$push(is_numeric(theta0, 2))
    errors$push(has_nonan(theta0, 2))
    errors$push(is_noninf(theta0, 2))
    # argument check results
    reportAssertions(errors)

    # function starting position
    mses <- sum((x - theta0)^2)/ length(x)

    return(mses)
}
