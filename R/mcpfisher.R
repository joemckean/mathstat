#' @title Fisher Multiple Comparison Procedure
#'
#' @description Computes the pairwise confidence intervals for the Fisher Multiple Comparison 
#' Procedure for a oneway layout.  This should be used if the overall F test of equality of
#' of the equality of the means rejects at level alpha. See page 528 of HMC and Example 9.4.1.
#'
#' @param y is the response vector and ind is the indicator (category, level) for the response.
#' ind must be integers from 1 to k, where k is the number of categories. 1-alpha is the confidence
#' level.   
#'
#' @return returns the Fisher Multiple Comparison confidence intervals for all pairwise
#' multiple comparisons of the means.
#'
#'
#' @references Hogg, R. McKean, J. Craig, A. (2018) Introduction to Mathematical
#' Statistics, 8th Ed. Boston: Pearson.
#'

mcpfisher <-
function(y,ind,alpha=.05){
       k <- max(ind)
       alphap = alpha/choose(k,2)
       tab <- c()
       fit <- lm(y~factor(ind))
       see <- summary(fit)
       sig <- see$sigma
       df <- length(y)-k
       fstat <- see$fstatistic[1]
       pf <- 1 - pf(fstat,(k-1),df)
       ftest <- c(fstat,pf)
       l <- choose(k,2)
       tc <- abs(qt(alpha/2,df))

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
        list(ftest=ftest,tab=tab)
}
