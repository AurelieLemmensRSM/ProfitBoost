% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verbeke_approach.R
\name{verbeke}
\alias{verbeke}
\title{Verbeke's profit margin maximization}
\usage{
verbeke(y, scores, m, delta, increment, gamma, c = 0, A = 0, plot = TRUE)
}
\arguments{
\item{y}{data}

\item{scores}{predicted scores provided by the model}

\item{m}{revenue}

\item{delta}{action cost (default: 12)}

\item{increment}{granularity level}

\item{gamma}{success rate of the incentive (estimated as ATE)}

\item{c}{cost of contact (default: 0)}

\item{A}{Fixed costs (default: 0)}

\item{plot}{Plot the results? (default: FALSE)}
}
\value{
A list containing campaign profit curve, target sizes,
maximum profit and related variables.

\itemize{
\item campaign.profit.curve
\item target.sizes
\item maxprofit
\item targetsize.maxprofit
\item increment
}
}
\description{
Verbeke's approach to maximize profit margin.
}
\details{
Formula:

profit = N\emph{alpha}((gamma\emph{CLV+delta}(1-gamma))\emph{beta0}lambda-delta-c)-A

Where:

N = number of customers in the customer base
alpha = proportion targeted (target size in \%) - vary to find max profit
beta0 = proportion of true would-be churners in customer base
scores = predicted scores provided by the model
delta = action cost = 12
gamma = success rate of the incentive - > estimated as ATE
c = cost of contact = 0
CLV = average customer lifetime value
A = fixed costs = 0
}
