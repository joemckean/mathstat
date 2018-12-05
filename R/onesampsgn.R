#' @title One-Sample Sign Analysis
#'
#' @description Calculates a distribution free confidence interval for a sample
#' median by deriving for the 100pth distribution percentile of a continuous random
#' variable.
#' .
#' @param x Sample of size 'n'
#' @param test Computes a two-sided sign test about theta0. If \code{test} is true,
#'  then H_0: theta = theta0 vs. H_1: theta != theta0 is conducted.
#' @param alt Computes a one-sided sign test for theta0. If \code{alt} equals 1,
#' then the test H_1: theta > theta0 is conducted. If \code{alt} equals -1, then
#' H_1: theta < theta0 is conducted.
#' @param theta0 Variance of sample \emph{x}
#' @param alpha Level of significance
#' @param maktable If true provides more descriptive output for x
#' @param plotb If true a boxplot is produced
#'
#' @details Computes the solution to Example 1.3.3 (8th Edition)
#'
#' @return The probability \code{bday} of at least two people having the same
#' birthdays
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to Mathematical
#' Statistics, 8th Ed. Boston: Pearson.
#'
#' @examples
#' x <- c(56, 70, 89, 94, 96, 101, 102, 102, 102, 105, 106, 108, 110, 113, 116)
#' # Confidence interval for the median:
#' onesampsgn(x, alpha = 0.12)
#'
#' @importFrom checkmate makeAssertCollection reportAssertions
#'
#' @export onesampsgn
# Test of H_0: theta = theta0

onesampsgn <- function(x, test = FALSE, alt = 0, theta0 = 0, alpha = 0.05, 
  maktable = TRUE, plotb = FALSE) {
  
  # checking arguments
  errors <- makeAssertCollection()
  # argument 1 x
  errors$push(is_manyelement(x, 1))
  errors$push(is_numeric(x, 1))
  errors$push(is_numvector(x, 1))
  errors$push(has_nonan(x, 1))
  errors$push(has_noinf(x, 1))
  # argument 2 test
  errors$push(is_oneelement(test, 2))
  errors$push(is_logical(test, 2))
  # argument 3 alt
  errors$push(is_oneelement(alt, 3))
  errors$push(is_numeric(alt, 3))
  errors$push(has_nonan(alt, 3))
  errors$push(is_noninf(alt, 3))
  # argument 4 theta0
  errors$push(is_oneelement(theta0, 4))
  errors$push(is_numeric(theta0, 4))
  errors$push(has_nonan(theta0, 4))
  errors$push(is_noninf(theta0, 4))
  # argument 5 alpha
  errors$push(is_oneelement(alpha, 5))
  errors$push(is_numeric(alpha, 5))
  errors$push(has_nonan(alpha, 5))
  errors$push(is_noninf(alpha, 5))
  # argument 6 maktable
  errors$push(is_oneelement(maktable, 6))
  errors$push(is_logical(maktable, 6))
  # argument 7 plotb
  errors$push(is_oneelement(plotb, 7))
  errors$push(is_logical(plotb, 7))
  # argument check results
  reportAssertions(errors)
  
  # Check for alpha
  if (alpha < 0 || alpha >= 1) {
    stop(gettext("input argument 'alpha' must be between zero and one"))
  }
  
  # Function starting point
  n <- length(x)
  
  ind <- rep(0, n)
  ind[x == theta0] <- 1
  
  n <- n - sum(ind)
  
  if (test) {
    ts <- sum(sign(x - theta0))
    
    zp <- (ts - 1)/sqrt(n)
    zn <- (ts + 1)/sqrt(n)
    
    if (alt == 1) {
      pval <- 1 - pnorm(zp)
      zs <- zp
    } else if (alt == -1) {
      pval <- pnorm(zn)
      zs <- zn
    } else if (alt == 0) {
      if (ts >= 0) {
        pval <- 2 * (1 - pnorm(zp))
        
        zs <- zp
      } else {
        pval <- 2 * pnorm(zn)
        zs <- zn
      }
    }
  }
  
  # Estimation
  n <- length(x)
  est <- median(x)
  
  xs <- sort(x)
  
  crit <- -qnorm(alpha/2)
  
  cut <- round((n/2) - crit * sqrt(n/4) - 0.5)
  
  if (cut < 0) {
    cut <- 0
  }
  
  lci <- xs[cut + 1]
  uci <- xs[n - cut]
  
  acconf <- 1 - 2 * pbinom(cut, n, 1/2)
  
  tau <- (sqrt(n) * (uci - lci))/(2 * crit)
  
  if (maktable) {
    if (test) {
      cat("\n")
      cat("Results for the Sign procedure", "\n")
      
      if (alt == 0) {
        cat("Test of theta =", theta0, " versus theta not equal to ", 
          theta0, "\n")
      }
      
      if (alt == 1) {
        cat("Test of theta =", theta0, " versus theta greater than ", 
          theta0, "\n")
      }
      
      if (alt == -1) {
        cat("Test of theta =", theta0, " versus theta less than ", 
          theta0, "\n")
      }
      
      cat("Test stat. S is", ts, " Standardized (z) Test-Stat.", 
        zs, "p-value", pval, "\n")
      
      cat("\n")
    }
    
    cat("Estimate ", est, " SE is ", tau/sqrt(n), "\n")
    
    pct <- 100 * (1 - alpha)
    
    cat(pct, "%", "Confidence Interval is ", "(", lci, ",", uci, 
      ")", "\n")
    
    cat("    Actual Confidence  is ", acconf, "\n")
    
    cat("Estimate of the scale parameter tau", tau, "\n")
    cat("\n", "This CI is the asymptotic form, see Hettmansperger and McKean (2011)", 
      "\n")
  }
  
  if (plotb) {
    boxplot(x, notch = TRUE)
    title(main = "95% Notched Boxplot of Data")
  }
  
  if (test == TRUE) {
    return(list(ts = ts, zs = zs, pval = pval, est = est, lci = lci, 
      uci = uci, acconf = acconf, tau = tau))
  } else {
    return(list(est = est, lci = lci, uci = uci, acconf = acconf, 
      tau = tau))
  }
}
