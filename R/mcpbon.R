#' @title Bonferroni Multiple Comparison Procedure
#'
#' @description Computes the Bonferroni Multiple Comparison Procedure for
#' a oneway layout.  See page 526 of HMC and Example 9.4.1.
#'
#' @param y is the response vector and ind is the indicator (category, level) for the response.
#' ind must be integers from 1 to k, where k is the number of categories. 1-alpha is the confidence
#' level.   Recall that the bonferroni procedure is conservative; i.e., overall confidence
#' is greater than or equal to 1 - alpha.
#'
#' @return returns the Bonferroni Multiple Comparison confidence intervals for all pairwise
#' multiple comparisons of the means.
#'
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to Mathematical
#' Statistics, 8th Ed. Boston: Pearson.
#'

mcpbon <-
function(y,ind,alpha=.05){
       k <- max(ind)
       alphap = alpha/choose(k,2)
       tab <- c()
       fit <- lm(y~factor(ind))
       sig <- summary(fit)$sigma
       df <- length(y)-k
       l <- choose(k,2)
       tc <- abs(qt(alpha/(2*l),df))

       for(j in 1:(k-1)){
            for(jp in (j+1):k){
                 see <- t.test(y[ind==j],y[ind==jp],conf.level=(1-alphap))
                 mu1 <- see$estimate[1]
                 mu2 <- see$estimate[2]
                 n1 <- length(ind[ind==j])
                 n2 <- length(ind[ind==jp])
                 est <- mu1 - mu2
                 se <- sig*sqrt((1/n1)+(1/n2))
                 err <- tc*se
                 lb <- est - err
                 ub <- est + err
                 rt <- c(j,jp,mu1,mu2,est,se,err,lb,ub)
                 tab <- rbind(tab,rt)
             }
        }
        colnames(tab) <- c("j","jp","muj","mujp","diff","se","err","lb","ub")
        rownames(tab) <- rep(" ",l)
        return(tab)
}
