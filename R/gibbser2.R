#' @title Gibbs Sampler
#'
#' @description  This function computes the Gibbs sampler routine for the situation
#' described in Example 11.3.2 on page 677. The input parameter alpha
#' is the parameter in the joint distribution of the random vector (X, Y) which
#' is given in the example. The value of m is the number of initial runs of the
#' sampler for achieving equilibrium, then the next (n * m)í°€m runs are recorded and returned
#' in the R vectors x2 and y2. The paired items (x2,i) \& (y2,i) are the variates for
#' random vector (X, Y).
#'
#' @param alpha test statistic applied in algorithm
#' @param m length of returned variables x1 and y1
#' @param n length of returned variables x2 and y2
#'
#' @return A list of two streams of iid random variables. The first pair is of
#' length \emph{m} and the second of length \emph{n}.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' gibbser2(alpha = 10, m = 3000, n = 6000)
#' gibbser2(alpha = 12, m = 4000, n = 10000)
#'
#'  @seealso gibbser3() for more details on Monte Carlo techniques used for
#' integrating in Bayesian inference
#'
#'@export gibbser2

gibbser2 <- function(alpha = 0,
                     m = 0,
                     n = 0) {

    # checking arguments
    errors <- makeAssertCollection()
    # argument 1 alpha
    errors$push(has_nonan(alpha, 1))
    errors$push(has_noinf(alpha, 1))
    errors$push(is_nonzero(alpha, 1))
    errors$push(is_positive(alpha, 1))
    errors$push(is_numeric(alpha, 1))
    errors$push(is_oneelement(alpha, 1))
    # argument 2 m
    errors$push(has_nonan(m, 2))
    errors$push(has_noinf(m, 2))
    errors$push(is_nonzero(m, 2))
    errors$push(is_positive(m, 2))
    errors$push(is_numeric(m, 2))
    errors$push(is_oneelement(m, 2))
    errors$push(is_smaller(m, n, 2, 3))
    # argument 3 n
    errors$push(has_nonan(n, 3))
    errors$push(has_noinf(n, 3))
    errors$push(is_nonzero(n, 3))
    errors$push(is_positive(n, 3))
    errors$push(is_numeric(n, 3))
    errors$push(is_oneelement(n, 3))
    # argument check results
    reportAssertions(errors)

    x0 <- 1

    yc <- rep(0, m + n)
    xc <- c(x0, rep(0, (m - 1) + n))

    for(i in 2:(m+n)){
        yc[i] <- rgamma(1, alpha + xc[i - 1], 2)
        xc[i] <- rpois(1, yc[i])
    }

    y1 <- yc[1:m]
    y2 <- yc[(m + 1):(m + n)]

    x1 <- xc[1:m]
    x2 <- xc[(m + 1):(m + n)]

    return(list(y1=y1,
                y2=y2,
                x1=x1,
                x2=x2))

}
