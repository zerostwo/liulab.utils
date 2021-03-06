% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculation.R
\name{calculate_correlation}
\alias{calculate_correlation}
\title{Calculate the correlation of two features}
\usage{
calculate_correlation(
  adata,
  feature.x,
  feature.y,
  cutoff.x = -Inf,
  cutoff.y = -Inf,
  method = "pearson"
)
}
\arguments{
\item{adata}{Expression matrix, rows are cells and columns are features}

\item{feature.x}{first feature}

\item{feature.y}{second feature}

\item{cutoff.x}{Threshold for feature.x value}

\item{cutoff.y}{Threshold for feature.y value}

\item{method}{a character string indicating which correlation coefficient is
to be used for the test. One of "pearson", "kendall", or "spearman",
can be abbreviated.}
}
\value{
a tibble objects
}
\description{
Calculate the correlation of two features
}
