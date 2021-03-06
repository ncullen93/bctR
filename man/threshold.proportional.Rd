% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Utility.R
\name{threshold.proportional}
\alias{threshold.proportional}
\title{Threshold Proportional}
\usage{
threshold.proportional(W, p, copy = F)
}
\arguments{
\item{W}{: an R Matrix - Weighted Connectivity Matrix}

\item{p}{: a float - Proportional Weight Threshold}

\item{copy}{: a boolean - Whether to modify in place or not}
}
\value{
W : Threshold Connectivity Matrix
}
\description{
This function "thresholds" the connectivity matrix by preserving a
proportion p (0<p<1) of the strongest weights. All other weights, and
all weights on the main diagonal (self-self connections) are set to 0.
}
\details{
Note: For data w/ negative numbers, we consider Absolute Value of weights.
}

