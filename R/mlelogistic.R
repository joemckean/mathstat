#' @title MLE Estimation of Location Based on a Logistic Distribution
#'
#' @description The following code obtains the mle for the location parameter theta,
#' assuming that the sample is drawn from a logistic distribution with mean (or median)
#' theta and scale parameter 1. The pdf is given in the expression (6.1.8), page 357.
#' The algorithm is a Newton-type procedure discussed in Examples 6.1.2 and 6.2.7.
#' To use it to estimate location for a given sample, standardize the sample first;
#' for example, divide the sample items by the sample standard deviation or the
#' interquartile range.
#'
#' @param x A numeric vector.
#' @param theta0 Called the one-step estimator. This estimator has the same asymptotic
#' distribution as the mle. The initial guess used for theta0 is the mean of the
#' input argument 'x'.
#' @param eps This quantity validates that the initial guess is a consistent estimator
#' of theta based on the inputted precision.
#'
#' @return \code{theta1} is the asymptotically efficient estimate of theta. \code{check}
#' is the difference between the initial guess of theta (i.e. \code{theta0}) and the
#' calculated estimate (i.e. \code{theta1}). \code{realnumstps} is the number of
#' iterations which was involved in obtaining \code{theta1}.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' # The following function generates a sample of size n from the logistic
#' # distribution with location theta and scale 1.
#'
#' rlogisticd <- function(n, theta) {
#'     u <- runif(n)
#'     rlogisticd <- log(u/(1 - u)) + theta
#'     return(rlogisticd)
#'     }
#'
#' # The following code generates a sample and fits it using mlelogistic
#'
#' n <- 50
#' theta <- 10
#' x <- rlogisticd(n, theta)
#'
#' mlelogistic(x)
#'
#' @export mlelogistic

mlelogistic <- function(x,                      # Gauss Newton Procedure
                        eps=.0001) {

    # checking arguments
    errors <- makeAssertCollection()
    # argument 1 x
    errors$push(has_nonan(x, 1))
    errors$push(is_numvector(x, 1))
    errors$push(has_noinf(x, 1))
    # argument 2 eps
    errors$push(is_oneelement(eps, 2))
    errors$push(is_numvector(eps, 2))
    errors$push(has_nonan(eps, 2))
    errors$push(has_noinf(eps, 2))
    errors$push(is_positive(eps, 2))
    # argument check results
    reportAssertions(errors)

    # Edge case check for input argument 'eps'
    if (eps < 0 || eps >= 1) {
        stop(gettext("input argument 'eps' must be between zero and one"))
    }

        theta0=mean(x)
        n <- length(x) # Sample size

        small <- 1.0 * 10^(-8)

        ic <- 0

        istop <- 0

        while(istop == 0){
            ic <- ic + 1

            expx <- exp(-(x - theta0))

            lprime <- n - 2 * sum(expx/(1 + expx))
            ldprime <- -2 * sum(expx/(1 + expx)^2)

            theta1 <- theta0 - (lprime/ ldprime)

            check <- abs(theta0 - theta1)/abs(theta0 + small)

            if(check < eps) {
                istop <- 1
            }

            theta0 <- theta1
        }

    return(list(theta1=theta1,
                check=check,
                realnumstps=ic))
}

