#' @title Confidence Interval and Predictive Interval for Linear Models
#'
#' @description Calculates the predicted response value associated with the
#' inputted hmat value. This function also calculates the predicted value's
#' confidence interval and predictive interval along with their standard errors.
#'
#' @param fit A linear model
#' @param hmat A vector used to store the value to be predicted for
#' @param alpha Level of significance to calculate the confidence interval
#' and predictive interval.
#'
#' @details A sample of this function's usage can be found in Example 9.6.1
#' from the book, Introduction to Mathematical Statistics (8th Edition)
#'
#' @return n x 1 numeric vector containing the random deviates.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' year <- men1500m$year
#' time <- men1500m$time
#' cipi(fit = lm(time ~ year), hmat = c(1, 2020), alpha = 0.05)
#'
#' @export cipi

cipi <- function(fit,
                 hmat,
                 alpha=0.05) {


    # checking arguments
    errors <- makeAssertCollection()
    # argument 1 fit
    errors$push(is_linearmodel(fit, 1))
    # argument 2 hmat
    errors$push(is_numeric(hmat, 2))
    # argument 3 alpha
    errors$push(has_nonan(alpha, 3))
    errors$push(has_noinf(alpha, 3))
    errors$push(is_numeric(alpha, 3))
    errors$push(is_oneelement(alpha, 3))
    # argument check results
    reportAssertions(errors)

    # Error handling hmat format
    if (length(hmat) != length(fit$coefficients)) {
        stop(gettext("length of hmat must match number of coefficients of linear model"))
    }

    # Edge case check for input argument 'alpha'
    if (alpha < 0 || alpha >= 1) {
        stop(gettext("input argument 'alpha' must be between zero and one"))
    }

    hmat <- as.matrix(hmat) # 1x2 matrix
    k <- length(hmat[, 1])

    n <- length(fit$resid)
    p <- length(fit$coef)
    df <- n - p

    sfit <- summary(fit)
    sig <- sfit$sigma
    vc <- sfit$cov.unscaled # 2x2 matrix
    beta <- as.matrix(fit$coef) # Converted to matrix

    tc <- abs(qt(alpha/ 2.0, df))

    matci <- matrix(rep(0, 4 * k), ncol=4)
    matpi <- matrix(rep(0, 4* k), ncol=4)

    for(i in 1:k) {
           h <- hmat[i, ]
           theta <- t(hmat) %*% beta


           seci <- sig * sqrt(t(hmat) %*% vc %*% hmat)

           lci <- theta - tc * seci
           uci <- theta + tc * seci

           matci[i, ] <- c(theta, seci, lci, uci)

           sepi <- sig * sqrt(1 + t(hmat) %*% vc %*% hmat)

           lpi <- theta - tc * sepi
           upi <- theta + tc * sepi

           matpi[i, ] <- c(theta, sepi, lpi, upi)
    }

    matcipi <- cbind(matci, matpi)
    colnames(matcipi) <- c("Pred",
                           "SECI",
                           "LCI",
                           "UCI",
                           "Pred",
                           "SEPI",
                           "LPI",
                           "UPI")

    return(matcipi)

}
data("men1500m", envir = environment(cipi))
