% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcpfisher.R
\name{mcpfisher}
\alias{mcpfisher}
\title{Fisher's Protected Least Significance Difference}
\usage{
mcpfisher(y, ind, alpha = 0.05)
}
\arguments{
\item{y}{Vector of the combined samples}

\item{ind}{Corresponding treatment vector}

\item{alpha}{Level of significance used for Fisher's Procedure}
}
\value{
This function returns a table to the user. The first two columns
\code{j} & \code{jp} are the specific levels (categories of cars) being compared.
\code{muj} & \code{mujp} are the means of two levels being compared. \code{diff}
is the difference in means between these levels. \code{se} is the standard
error for each comparison, \code{err} is the comparison's error estimate, and
\code{lb} & \code{ub} consist of the lower and upper bound for each comparison's
confidence interval.
}
\description{
Fisher's PLSD Multiple Comparison Procedure is a two-stage procedure.
For a specified level of significance (alpha), the first step consists of the
\emph{F}-test of the hypotheses of equal means. If the test is rejected at level
alpha, then the second stage will compute pairwise (1-alpha)100\% confidence
intervals.
}
\details{
An example of this Fisher's PLSD Multiple Comparison Procedure
can be found in Example 9.4.1 on page 529 of HMC (2018).
}
\examples{
speed <- fastcars$speed
car <- fastcars$ind
mcpfisher(speed, car)

}
\references{
Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
Statistics, 8th Ed. Boston: Pearson.
}
\seealso{
mcpbon() for details Bonferroni procedure for all pairwise comparisons
}
