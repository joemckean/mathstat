#' @title Fisher's Protected Least Significance Difference
#'
#' @description Fisher's PLSD Multiple Comparison Procedure is a two-stage procedure.
#' For a specified level of significance (alpha), the first step consists of the
#' \emph{F}-test of the hypotheses of equal means. If the test is rejected at level
#' alpha, then the second stage will compute pairwise (1-alpha)100\% confidence
#' intervals.
#'
#' @param y Vector of the combined samples
#' @param ind Corresponding treatment vector
#' @param alpha Level of significance used for Fisher's Procedure
#'
#' @details An example of this Fisher's PLSD Multiple Comparison Procedure
#' can be found in Example 9.4.1 on page 529 of HMC (2018).
#'
#' @return This function returns a table to the user. The first two columns
#' \code{j} \& \code{jp} are the specific levels (categories of cars) being compared.
#' \code{muj} \& \code{mujp} are the means of two levels being compared. \code{diff}
#' is the difference in means between these levels. \code{se} is the standard
#' error for each comparison, \code{err} is the comparison's error estimate, and
#' \code{lb} \& \code{ub} consist of the lower and upper bound for each comparison's
#' confidence interval.
#'
#' @references Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
#'             Statistics, 8th Ed. Boston: Pearson.
#'
#' @seealso mcpbon() for details Bonferroni procedure for all pairwise comparisons
#'
#' @examples
#' load('./data/fastcars.rda')
#' speed <- fastcars$speed
#' car <- fastcars$ind
#' mcpfisher(speed, car)
#'
#' @export mcpfisher

mcpfisher <- function(y,
                      ind,
                      alpha=.05) {

    # checking arguments
    errors <- makeAssertCollection()
    # argument 1 y
    errors$push(has_noinf(y, 1))
    errors$push(is_numeric(y, 1))
    errors$push(has_nonan(y, 1))
    # argument 2 ind
    errors$push(has_noinf(ind, 2))
    errors$push(has_nonan(ind, 2))
    # argument 3 alpha
    errors$push(has_nonan(alpha, 3))
    errors$push(has_noinf(alpha, 3))
    errors$push(is_nonzero(alpha, 3))
    errors$push(is_positive(alpha, 3))
    errors$push(is_numeric(alpha, 3))
    errors$push(is_oneelement(alpha, 3))
    # argument check results
    reportAssertions(errors)

    if (alpha < 0 || alpha > 1) {
        stop(gettext("input object 'alpha' must be between 0 and 1"))
    }
    uvec <- unique(as.numeric(ind))
    cartype <- unique(ind)

    k <- max(uvec)

    alphap <-  alpha/choose(k, 2)

    tab <- c()

    fit <- lm(y ~ factor(ind))
    see <- summary(fit)
    sig <- see$sigma

    df <- length(y) - k

    fstat <- see$fstatistic[1]

    pf <- 1 - pf(fstat,(k - 1), df)
    ftest <- c(fstat, pf)

    l <- choose(k, 2)
    tc <- abs(qt(alpha/ 2, df))

    for(j in 1:(k - 1)) {
        for(jp in (j + 1):k) {
            see <- t.test(y[uvec==j], y[uvec==jp], conf.level=(1 - alphap))

            mu1 <- see$estimate[1]
            mu2 <- see$estimate[2]

            n1 <- length(ind[uvec==j])
            n2 <- length(ind[uvec==jp])

            est <- mu1 - mu2

            se <- sig * sqrt((1/ n1) + (1/ n2))
            err <- tc * se

            lb <- est - err
            ub <- est + err

            rt <- c(j,
                    jp,
                    mu1,
                    mu2,
                    est,
                    se,
                    err,
                    lb,
                    ub)

            tab <- rbind(tab, rt)
        }
    }

    colnames(tab) <- c("j",
                       "jp",
                       "muj",
                       "mujp",
                       "diff",
                       "se",
                       "err",
                       "lb",
                       "ub")

    # Converting table to dataframe: Allows car names to be returned
    tab2 <- as.data.frame(tab, row.names = " ")

    for (i in 1:nrow(tab2)) {
        tab2$j[tab2$j == i] <- paste(cartype[i])
        tab2$jp[tab2$jp == i] <- paste(cartype[i])
    }

    return(list(ftest=ftest,tab=tab2))
}
