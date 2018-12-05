#' @title Bonferroni's Multiple Comparisons Procedure
#'
#' @description Bonferroni's approach to Multiple Comparisons Procedure that uses
#' the method of complements, DeMorgan's Laws and Boole's inequality. This procedure
#' is robust and can be used in many settings rather than just one-way design.
#'
#' @param y Vector of the combined samples of size n
#' @param ind Corresponding treatment vector of size n
#' @param alpha Level of significance used for Bonferroni Procedure
#'
#' @details Computes the Bonferroni confidence intervals as seen in Example
#' 9.4.1 on page 529.
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
#' @seealso mcpfisher() for details about Fisher's PLSD Multiple Comparison Procedure
#'
#' @examples
#' speed <- fastcars$speed
#' car <- fastcars$ind
#' mcpbon(speed, car)
#'
#' @export mcpbon

mcpbon <- function(y, ind, alpha = 0.05) {
  
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
  
  alphap <- alpha/choose(k, 2)
  
  tab <- c()
  car1 <- c()
  car2 <- c()
  
  fit <- lm(y ~ factor(ind))
  sig <- summary(fit)$sigma
  
  df <- length(y) - k
  
  l <- choose(k, 2)
  
  tc <- abs(qt(alpha/(2 * l), df))
  
  for (j in 1:(k - 1)) {
    for (jp in (j + 1):k) {
      see <- t.test(y[uvec == j], y[uvec == jp], conf.level = (1 - 
        alphap))
      
      car1 <- cartype[j]
      car2 <- cartype[jp]
      
      mu1 <- see$estimate[1]
      mu2 <- see$estimate[2]
      
      n1 <- length(ind[uvec == j])
      n2 <- length(ind[uvec == jp])
      
      est <- mu1 - mu2
      
      se <- sig * sqrt((1/n1) + (1/n2))
      err <- tc * se
      
      lb <- est - err
      ub <- est + err
      
      rt <- c(j, jp, mu1, mu2, est, se, err, lb, ub)
      
      
      tab <- rbind(tab, rt)
    }
  }
  
  colnames(tab) <- c("j", "jp", "muj", "mujp", "diff", "se", "err", "lb", 
    "ub")
  
  # Converting table to dataframe: Allows car names to be returned
  tab2 <- as.data.frame(tab, row.names = " ")
  
  for (i in 1:nrow(tab2)) {
    tab2$j[tab2$j == i] <- paste(cartype[i])
    tab2$jp[tab2$jp == i] <- paste(cartype[i])
  }
  
  
  return(tab2)
}

data("fastcars", envir = environment(mcpbon))
