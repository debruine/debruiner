% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{steiger}
\alias{steiger}
\title{Steiger's Z}
\usage{
steiger(r12, r13, r23, n, tails = 2)
}
\arguments{
\item{r12}{the correlation between variables 1 and 2}

\item{r13}{the correlation between variables 1 and 3}

\item{r23}{the correlation between variables 2 and 3}

\item{n}{the number of observations}

\item{tails}{whether to report a one-tailed or two-tailed p-value}
}
\value{
list
}
\description{
\code{steiger} Test for significant difference between two correlations on the same sample.
}
\examples{
steiger(.4, .3, .2, 20) # z=0.3583792, p=0.7200596
}
