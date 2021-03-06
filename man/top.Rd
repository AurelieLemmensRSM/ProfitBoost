% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_utility.R
\name{top}
\alias{top}
\title{Top}
\usage{
top(y, p, share)
}
\arguments{
\item{y}{a vector of length n containing the actual churn outcome
of all customers}

\item{p}{a vector of length n containing the estimated probability or score
to churn of all customers}

\item{share}{fraction (\%) of customers to targeted, e.g. share = 0.1
is the top-decile}
}
\value{
the top-x\% lift
}
\description{
This function calculates the top x\% lift of a churn model,
given the estimated probabilities to churn.
}
