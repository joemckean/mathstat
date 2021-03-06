% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fp2.R
\name{fp2}
\alias{fp2}
\title{F-Distribution Table for Upper 0.05 Critical Points}
\usage{
fp2()
}
\value{
\code{xmat} is the F-distribution table for the 95th quantile
}
\description{
Produces the F-distribution table for the 0.95 quantile
of the F-distribution.
}
\details{
This table more specifically produces the F-distribution table for
when alpha equals 0.05. The initial row indicates the degrees of freedom
in the numerator for values greater than 9 and the inital column indicates degrees
of freedom in the denominator. See page 712 for more details.
}
\examples{
fp2()

}
\references{
Hogg, R., McKean, J., Craig, A. (2018) Introduction to Mathematical
Statistics, 8th Ed. Boston: Pearson.
}
\seealso{
fp1() for the F-distribution table of the 0.95 quantile with
the numerator having less than ten degrees of freedom, as well as fp3() and
fp4() for the F-distribution table of the 0.99 quantile.
}
