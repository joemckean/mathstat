% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dcn.R
\name{dcn}
\alias{dcn}
\title{PDF of Contaminated Normal}
\usage{
dcn(w, eps, sigma_c)
}
\arguments{
\item{w}{Variable bound on random variable of interest W.}

\item{eps}{Proportion of contamination. Usually between 0 and 0.25.}

\item{sigma_c}{Standard deviation of contaminated part.
Should be greater than 1.}
}
\value{
Value of PDF at w for a contaminated normal.
}
\description{
Computes the PDF of the standard normals for the PDF of W at w,
represented as \eqn{P(W = w)} where W is the random variable of interest.
See expression 3.4.19 on page 194 of the book.
}
\examples{
# Example where variables are set then passed in as parameters.
w <- 10
eps <- 0.5
sigma_c <- 0.25
p <- dcn(w, eps, sigma_c)

# Example where parameters are passed in as values.
p <- dcn(10, 0.5, 0.25)

}
\references{
Hogg, R. McKean, J. Craig, A (2018) Introduction to
Mathematical Statistics, 8th Ed. Boston: Pearson
}
